{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Language.SexpGrammar.Base
  ( position
  -- * Atoms
  , real
  , double
  , int
  , integer
  , string
  , symbol
  , keyword
  , sym
  , kwd
  -- * Lists
  , List
  , list
  , bracketList
  , braceList
  , el
  , rest
  -- * Property lists
  , PropertyList
  , props
  , key
  , optKey
  , (.:)
  , (.:?)
  , restKeys
    -- * Quotes, antiquotes, etc
  , Prefix (..)
  , prefixed
  , quoted
  , hashed
  ) where

import Control.Category ((>>>))

import Data.Coerce
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Base
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Language.Sexp.Located

-- Setup code for doctest.
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Language.SexpGrammar (encodeWith)

----------------------------------------------------------------------

ppBrief :: Sexp -> Text
ppBrief = TL.toStrict . \case
  atom@Atom{} ->
    TL.decodeUtf8 (encode atom)
  other ->
    let pp = TL.decodeUtf8 (encode other)
    in if TL.length pp > 25
       then TL.take 25 pp <> "..."
       else pp

ppKey :: Text -> Text
ppKey kw = "keyword :" <> kw

----------------------------------------------------------------------

-- | Key\/value pairs of a property list that is being parsed/constructed.
newtype PropertyList = PropertyList [(Text, Sexp)]

-- | Elements of a list that is being parsed/constructed.
newtype List = List [Sexp]

----------------------------------------------------------------------

-- | Extract\/inject a position from\/to a 'Sexp'.
position :: Grammar Position (Sexp :- t) (Position :- Sexp :- t)
position = Iso
  (\(s@(Fix (Compose (p :< _))) :- t) -> p :- s :- t)
  (\(p :- Fix (Compose (_ :< s)) :- t) -> Fix (Compose (p :< s)) :- t)


locate :: Grammar Position (Sexp :- t) (Sexp :- t)
locate =
  position >>>
  onHead Locate >>>
  Iso (\(_ :- t) -> t) (\t -> dummyPos :- t)


atom :: Grammar Position (Sexp :- t) (Atom :- t)
atom = locate >>> partialOsi
  (\case
      Atom a -> Right a
      other -> Left (expected "atom" <> unexpected (ppBrief other)))
  Atom


beginParenList :: Grammar Position (Sexp :- t) (List :- t)
beginParenList = locate >>> partialOsi
  (\case
      ParenList a -> Right (List a)
      other -> Left (expected "list" <> unexpected (ppBrief other)))
  (ParenList . coerce)


beginBracketList :: Grammar Position (Sexp :- t) (List :- t)
beginBracketList = locate >>> partialOsi
  (\case
      BracketList a -> Right (List a)
      other -> Left (expected "bracket list" <> unexpected (ppBrief other)))
  (BracketList . coerce)


beginBraceList :: Grammar Position (Sexp :- t) (List :- t)
beginBraceList = locate >>> partialOsi
  (\case
      BraceList a -> Right (List a)
      other -> Left (expected "brace list" <> unexpected (ppBrief other)))
  (BraceList . coerce)


endList :: Grammar Position (List :- t) t
endList = Flip $ PartialIso
  (\t -> List [] :- t)
  (\(List lst :- t) ->
      case lst of
        [] -> Right t
        (el:_rest) -> Left (unexpected (ppBrief el)))


-- | Parenthesis list grammar. Runs a specified grammar on a
-- sequence of S-exps in a parenthesized list.
--
-- >>> let grammar = list (el symbol >>> el int) >>> pair
-- >>> encodeWith grammar ("foo", 42)
-- Right "(foo 42)"
list :: Grammar Position (List :- t) (List :- t') -> Grammar Position (Sexp :- t) t'
list g = beginParenList >>> Dive (g >>> endList)


-- | Bracket list grammar. Runs a specified grammar on a
-- sequence of S-exps in a bracketed list.
--
-- >>> let grammar = bracketList (rest int)
-- >>> encodeWith grammar [2, 3, 5, 7, 11, 13]
-- Right "[2 3 5 7 11 13]"
bracketList :: Grammar Position (List :- t) (List :- t') -> Grammar Position (Sexp :- t) t'
bracketList g = beginBracketList >>> Dive (g >>> endList)


-- | Brace list grammar. Runs a specified grammar on a
-- sequence of S-exps in a list enclosed in braces.
--
-- >>> let grammar = braceList (props (key "x" real >>> key "y" real)) >>> pair
-- >>> encodeWith grammar (3.1415, -1)
-- Right "{:x 3.1415 :y -1}"
braceList :: Grammar Position (List :- t) (List :- t') -> Grammar Position (Sexp :- t) t'
braceList g = beginBraceList >>> Dive (g >>> endList)

----------------------------------------------------------------------

-- | Element of a sequence grammar. Runs a specified grammar on a next
-- element of a sequence. The underlying grammar can produce zero or
-- more values on the stack.
--
-- E.g.:
--
-- * @el (sym "lambda")@ consumes a symbol \"lambda\" and produces no
--   values on the stack.
--
-- * @el symbol@ consumes a symbol and produces a 'Text' value
--   corresponding to the symbol.
el :: Grammar p (Sexp :- t) t' -> Grammar p (List :- t) (List :- t')
el g = coerced (Flip cons >>> onTail g >>> Step)


-- | The rest of a sequence grammar. Runs a specified grammar on each
-- of remaining elements of a sequence and collect them. Expects zero
-- or more elements in the sequence.
--
-- >>> let grammar = list (el (sym "check-primes") >>> rest int)
-- >>> encodeWith grammar [2, 3, 5, 7, 11, 13]
-- Right "(check-primes 2 3 5 7 11 13)"
rest
  :: (forall t. Grammar p (Sexp :- t) (a :- t))
  -> Grammar p (List :- t) (List :- [a] :- t)
rest g =
  iso coerce coerce >>>
  onHead (Traverse (sealed g >>> Step)) >>>
  Iso (\a -> List [] :- a) (\(_ :- a) -> a)

----------------------------------------------------------------------

beginProperties
  :: Grammar p (List :- t) (List :- PropertyList :- t)
beginProperties = Flip $ PartialIso
  (\(List rest :- PropertyList alist :- t) ->
      List (concatMap (\(k, v) -> [Atom (AtomSymbol (':' `TS.cons` k)), v]) alist ++ rest) :- t)
  (\(List lst :- t) ->
      let (rest, alist) = takePairs lst [] in
      Right (List rest :- PropertyList (reverse alist) :- t))
  where
    takePairs :: [Sexp] -> [(Text, Sexp)] -> ([Sexp], [(Text, Sexp)])
    takePairs (Atom (AtomSymbol k) : v : rest) acc =
      case TS.uncons k of
        Just (':', k') -> takePairs rest ((k', v) : acc)
        _              -> (Atom (AtomSymbol k) : v : rest, acc)
    takePairs other acc = (other, acc)


endProperties
  :: Grammar p t (PropertyList :- t)
endProperties = PartialIso
  (\t -> PropertyList [] :- t)
  (\(PropertyList lst :- t) ->
      case lst of
        [] -> Right t
        ((k, _) : _rest) -> Left (unexpected (ppKey k)))


-- | Property list in a sequence grammar. Collects pairs of keywords
-- and S-expressions from remaining sequence elements and runs a
-- specified grammar on them. Expects zero or more pairs in the
-- sequence. If sequence of pairs interrupts with a non-keyword, the
-- rest of this sequence is left untouched.
--
-- Collected 'PropertyList' is then available for random-access lookup
-- combinators 'key', 'optKey', '.:', '.:?' or bulk extraction
-- 'restKeys' combinator.
--
-- >>> :{
--  let grammar = braceList (
--        props (key "real" real >>> key "img" real) >>> onTail pair >>> el (sym "/") >>>
--        props (key "real" real >>> key "img" real) >>> onTail pair) >>> pair
--  in encodeWith grammar ((0, -1), (1, 0))
-- :}
-- Right "{:real 0 :img -1 / :real 1 :img 0}"
props
  :: Grammar p (PropertyList :- t) (PropertyList :- t')
  -> Grammar p (List :- t) (List :- t')
props g = beginProperties >>> Dive (onTail (g >>> Flip endProperties))


-- | Property by a key grammar. Looks up an S-expression by a
-- specified key and runs a specified grammar on it. Expects the key
-- to be present.
--
-- Note: performs linear lookup, /O(n)/
key
  :: Text
  -> (forall t. Grammar p (Sexp :- t) (a :- t))
  -> Grammar p (PropertyList :- t) (PropertyList :- a :- t)
key k g =
  coerced (
    Flip (insert k (expected $ ppKey k)) >>>
    Step >>>
    onHead (sealed g) >>>
    swap)


-- | Optional property by a key grammar. Like 'key' but puts 'Nothing'
-- in correspondence to the missing key and 'Just' to the present.
--
-- Note: performs linear lookup, /O(n)/
optKey
  :: Text
  -> (forall t. Grammar p (Sexp :- t) (a :- t))
  -> Grammar p (PropertyList :- t) (PropertyList :- Maybe a :- t)
optKey k g =
  coerced (Flip (insertMay k) >>>
    Step >>>
    onHead (Traverse (sealed g)) >>>
    swap)

infix 3 .:
infix 3 .:?


-- | Property by a key grammar. Infix version of 'key'.
(.:)
  :: Text
  -> (forall t. Grammar p (Sexp :- t) (a :- t))
  -> Grammar p (PropertyList :- t) (PropertyList :- a :- t)
(.:) = key


-- | Optional property by a key grammar. Infix version of 'optKey'.
(.:?)
  :: Text
  -> (forall t. Grammar p (Sexp :- t) (a :- t))
  -> Grammar p (PropertyList :- t) (PropertyList :- Maybe a :- t)
(.:?) = optKey


-- | Remaining properties grammar. Extracts all key-value pairs and
-- applies a grammar on every element.
restKeys
  :: (forall t. Grammar p (Sexp :- Text :- t) (a :- t))
  -> Grammar p (PropertyList :- t) (PropertyList :- [a] :- t)
restKeys f =
  iso coerce coerce >>>
  onHead (Traverse (sealed (Flip pair >>> f) >>> Step)) >>>
  Iso (\a -> PropertyList [] :- a) (\(_ :- a) -> a)


----------------------------------------------------------------------
-- Atoms

-- | Grammar matching integer number atoms to 'Integer' values.
--
-- >>> encodeWith integer (2^100)
-- Right "1267650600228229401496703205376"
integer :: Grammar Position (Sexp :- t) (Integer :- t)
integer = atom >>> partialOsi
  (\case
      AtomNumber n | Right i <- (floatingOrInteger n :: Either Double Integer) -> Right i
      other -> Left (expected "integer" <> unexpected (ppBrief $ Atom other)))
  (AtomNumber . fromIntegral)


-- | Grammar matching integer number atoms to 'Int' values.
--
-- >>> encodeWith int (2^63)
-- Right "-9223372036854775808"
--
-- >>> encodeWith int (2^63-1)
-- Right "9223372036854775807"
int :: Grammar Position (Sexp :- t) (Int :- t)
int = integer >>> iso fromIntegral fromIntegral


-- | Grammar matching fractional number atoms to 'Scientific' values.
--
-- >>> encodeWith real (3.141592653589793^3)
-- Right "31.006276680299813114880451174049119330924860257"
real :: Grammar Position (Sexp :- t) (Scientific :- t)
real = atom >>> partialOsi
  (\case
      AtomNumber r -> Right r
      other -> Left (expected "real" <> unexpected (ppBrief $ Atom other)))
  AtomNumber


-- | Grammar matching fractional number atoms to 'Double' values.
--
-- >>> encodeWith double (3.141592653589793^3)
-- Right "31.006276680299816"
double :: Grammar Position (Sexp :- t) (Double :- t)
double = real >>> iso toRealFloat fromFloatDigits


-- | Grammar matching string literal atoms to 'Text' values.
--
-- >>> let grammar = list (el string >>> el int) >>> pair
-- >>> encodeWith grammar ("some-string", 42)
-- Right "(\"some-string\" 42)"
string :: Grammar Position (Sexp :- t) (Text :- t)
string = atom >>> partialOsi
  (\case
      AtomString s -> Right s
      other -> Left (expected "string" <> unexpected (ppBrief $ Atom other)))
  AtomString


-- | Grammar matching symbol literal atoms to 'Text' values.
--
-- >>> encodeWith symbol "some-symbol"
-- Right "some-symbol"
symbol :: Grammar Position (Sexp :- t) (Text :- t)
symbol = atom >>> partialOsi
  (\case
      AtomSymbol s -> Right s
      other -> Left (expected "symbol" <> unexpected (ppBrief $ Atom other)))
  AtomSymbol


-- | Grammar matching symbol literal atoms starting with \':\' to
-- 'Text' values without the colon char.
--
-- >>> encodeWith keyword "username"
-- Right ":username"
keyword :: Grammar Position (Sexp :- t) (Text :- t)
keyword = atom >>> partialOsi
  (\case
      AtomSymbol s | Just (':', k) <- TS.uncons s -> Right k
      other -> Left (expected "keyword" <>
                     unexpected (ppBrief $ Atom other)))
  (AtomSymbol . TS.cons ':')


-- | Grammar matching symbol literal atoms to a specified symbol.
--
-- >>> let grammar = list (el (sym "username") >>> el string)
-- >>> encodeWith grammar "Julius Caesar"
-- Right "(username \"Julius Caesar\")"
sym :: Text -> Grammar Position (Sexp :- t) t
sym s = atom >>> Flip (PartialIso
  (AtomSymbol s :-)
  (\(a :- t) ->
      case a of
        AtomSymbol s' | s == s' -> Right t
        other -> Left $ expected ("symbol " <> s) <>
                        unexpected (ppBrief $ Atom other)))


-- | Grammar matching symbol literal atoms to a specified symbol
-- prepended with \':\'.
--
-- >>> let grammar = list (el (kwd "password") >>> el int)
-- >>> encodeWith grammar 42
-- Right "(:password 42)"
kwd :: Text -> Grammar Position (Sexp :- t) t
kwd s =
  let k = TS.cons ':' s
  in atom >>> Flip (PartialIso
       (AtomSymbol k :-)
       (\(a :- t) ->
           case a of
             AtomSymbol s' | k == s' -> Right t
             other -> Left $ expected (ppKey s) <> unexpected (ppBrief $ Atom other)))


prefix :: Prefix -> Grammar Position (Sexp :- t) (Sexp :- t)
prefix m = locate >>> partialOsi
  (\case
      Modified m' a | m' == m -> Right a
      other -> Left (expected (ppBrief (Modified m (Symbol "-prefixed"))) <> unexpected (ppBrief other)))
  (Modified m)

-- | Grammar matching a prefixed S-expression, runs a sub-grammar on a
-- @Sexp@ under the hash prefix.
--
-- > encodeWith (hashed symbol) "foo" ≡ "#foo"
hashed :: Grammar Position (Sexp :- t) (a :- t) -> Grammar Position (Sexp :- t) (a :- t)
hashed g = prefix Hash >>> g

-- | Grammar matching a prefixed S-expression, runs a sub-grammar on a
-- @Sexp@ under the quotation.
--
-- > encodeWith (quoted symbol) "foo" ≡ "'foo"
quoted :: Grammar Position (Sexp :- t) (a :- t) -> Grammar Position (Sexp :- t) (a :- t)
quoted g = prefix Quote >>> g


-- | Grammar matching a prefixed S-expression, runs a sub-grammar on a
-- @Sexp@ under the prefix.
--
-- > encodeWith (prefixed Backtick symbol) "foo" ≡ "`foo"
prefixed :: Prefix -> Grammar Position (Sexp :- t) (a :- t) -> Grammar Position (Sexp :- t) (a :- t)
prefixed m g = prefix m >>> g
