{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}

module Language.SexpGrammar.Base
  ( position
  -- * Atoms
  , bool
  , real
  , double
  , int
  , integer
  , string
  , symbol
  , keyword
  , sym
  , kwd
  , enum
  -- * Lists
  , List
  , list
  , vect
  , bracelist
  , el
  , rest
  -- * Property lists
  , PropertyList
  , dict
  , props
  , key
  , keyMay
  , (.:)
  , (.:?)
  , restKeys
  ) where

import Control.Category ((>>>))

import Data.Char
import Data.Coerce
import Data.Data
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Base
import Data.List (intercalate)
import Data.List.Split
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Language.Sexp.Located


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
ppKey kw = "keyword " <> kw

----------------------------------------------------------------------

newtype PropertyList = PropertyList [(Text, Sexp)]

newtype List = List [Sexp]

----------------------------------------------------------------------

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


list_ :: Grammar p (Sexp :- t) (List :- t)
list_ = partialOsi
  (\case
      ParenList a -> Right (List a)
      other -> Left (expected "list" <> unexpected (ppBrief other)))
  (ParenList . coerce)


emptyList :: Grammar p t (List :- t)
emptyList = PartialIso
  (\t -> List [] :- t)
  (\(List lst :- t) ->
      case lst of
        [] -> Right t
        (el:_rest) -> Left (unexpected (ppBrief el)))


list :: Grammar Position (List :- t) (List :- t') -> Grammar Position (Sexp :- t) t'
list g = locate >>> list_ >>> Dive (g >>> Flip emptyList)


vector_ :: Grammar p (Sexp :- t) (List :- t)
vector_ = partialOsi
  (\case
      BracketList a -> Right (List a)
      other -> Left (expected "vector" <> unexpected (ppBrief other)))
  (BracketList . coerce)


vect :: Grammar Position (List :- t) (List :- t') -> Grammar Position (Sexp :- t) t'
vect g = locate >>> vector_ >>> Dive (g >>> Flip emptyList)


bracelist_ :: Grammar p (Sexp :- t) (List :- t)
bracelist_ = partialOsi
  (\case
      BraceList a -> Right (List a)
      other -> Left (expected "brace-list" <> unexpected (ppBrief other)))
  (BraceList . coerce)


bracelist :: Grammar Position (List :- t) (List :- t') -> Grammar Position (Sexp :- t) t'
bracelist g = locate >>> bracelist_ >>> Dive (g >>> Flip emptyList)


----------------------------------------------------------------------

el :: Grammar p (Sexp :- t) t' -> Grammar p (List :- t) (List :- t')
el g = coerced $ Flip cons >>> onTail g >>> Step


rest :: (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p (List :- t) (List :- [a] :- t)
rest g = iso coerce coerce >>> onHead (Traverse (sealed g >>> Step)) >>> Iso (\a -> List [] :- a) (\(_ :- a) -> a)

----------------------------------------------------------------------

dict :: Grammar Position (PropertyList :- t) (PropertyList :- t') -> Grammar Position (Sexp :- t) t'
dict g = bracelist (props g)


props_ :: Grammar p (List :- t) (List :- PropertyList :- t)
props_ = Flip $ PartialIso
  (\(List rest :- PropertyList alist :- t) ->
      List (concatMap (\(k, v) -> [Atom (AtomSymbol k), v]) alist ++ rest) :- t)
  (\(List lst :- t) ->
      let (rest, alist) = takePairs lst [] in
      Right (List rest :- PropertyList alist :- t))
  where
    takePairs :: [Sexp] -> [(Text, Sexp)] -> ([Sexp], [(Text, Sexp)])
    takePairs (Atom (AtomSymbol k) : v : rest) acc =
      case TS.uncons k of
        Just (':', _) -> takePairs rest ((k, v) : acc)
        _             -> (Atom (AtomSymbol k) : v : rest, acc)
    takePairs other acc = (other, acc)


props :: Grammar p (PropertyList :- t) (PropertyList :- t') -> Grammar p (List :- t) (List :- t')
props g = Dive $ props_ >>> onTail (g >>> Flip emptyProps)
  where
    emptyProps :: Grammar p t (PropertyList :- t)
    emptyProps = PartialIso
      (\t -> PropertyList [] :- t)
      (\(PropertyList lst :- t) ->
          case lst of
            [] -> Right t
            ((k, _) : _rest) -> Left (unexpected (ppKey k)))


key :: Text -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p (PropertyList :- t) (PropertyList :- a :- t)
key k g =
  let k' = TS.cons ':' k
  in coerced $ Flip (insert k' (expected $ ppKey k')) >>> Step >>> onHead (sealed g) >>> swap


keyMay :: Text -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p (PropertyList :- t) (PropertyList :- Maybe a :- t)
keyMay k g =
  let k' = TS.cons ':' k
  in coerced $ Flip (insertMay k') >>> Step >>> onHead (Traverse (sealed g)) >>> swap


(.:) :: Text -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p (PropertyList :- t) (PropertyList :- a :- t)
(.:) = key


(.:?) :: Text -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p (PropertyList :- t) (PropertyList :- Maybe a :- t)
(.:?) = keyMay


parallel
  :: (forall t. Grammar p (a :- t) (b :- t))
  -> (forall t. Grammar p (c :- t) (d :- t))
  -> Grammar p ((a, c) :- t) ((b, d) :- t)
parallel f g = Flip pair >>> onHead (sealed g) >>> onTail (onHead (sealed f))  >>> pair


restKeys
  :: (forall t. Grammar p (Text :- t) (a :- t))
  -> (forall t. Grammar p (Sexp :- t) (b :- t))
  -> Grammar p (PropertyList :- t) (PropertyList :- [(a, b)] :- t)
restKeys f g = iso coerce coerce >>> onHead (Traverse (sealed (parallel f g) >>> Step)) >>> Iso (\a -> PropertyList [] :- a) (\(_ :- a) -> a)


----------------------------------------------------------------------
-- Atoms

bool :: Grammar Position (Sexp :- t) (Bool :- t)
bool = atom >>> partialOsi
  (\case
      AtomSymbol "tt" -> Right True
      AtomSymbol "ff" -> Right False
      other   -> Left (expected "bool" <> unexpected (ppBrief $ Atom other)))
  (\case
      True  -> AtomSymbol "tt"
      False -> AtomSymbol "ff")


integer :: Grammar Position (Sexp :- t) (Integer :- t)
integer = atom >>> partialOsi
  (\case
      AtomNumber n | Right i <- (floatingOrInteger n :: Either Double Integer) -> Right i
      other -> Left (expected "integer" <> unexpected (ppBrief $ Atom other)))
  (AtomNumber . fromIntegral)


int :: Grammar Position (Sexp :- t) (Int :- t)
int = integer >>> iso fromIntegral fromIntegral


real :: Grammar Position (Sexp :- t) (Scientific :- t)
real = atom >>> partialOsi
  (\case
      AtomNumber r -> Right r
      other -> Left (expected "real" <> unexpected (ppBrief $ Atom other)))
  AtomNumber


double :: Grammar Position (Sexp :- t) (Double :- t)
double = real >>> iso toRealFloat fromFloatDigits


string :: Grammar Position (Sexp :- t) (Text :- t)
string = atom >>> partialOsi
  (\case
      AtomString s -> Right s
      other -> Left (expected "string" <> unexpected (ppBrief $ Atom other)))
  AtomString


symbol :: Grammar Position (Sexp :- t) (Text :- t)
symbol = atom >>> partialOsi
  (\case
      AtomSymbol s -> Right s
      other -> Left (expected "symbol" <> unexpected (ppBrief $ Atom other)))
  AtomSymbol


keyword :: Grammar Position (Sexp :- t) (Text :- t)
keyword = atom >>> partialOsi
  (\case
      AtomSymbol s | Just (':', k) <- TS.uncons s -> Right k
      other -> Left (expected "keyword" <> unexpected (ppBrief $ Atom other)))
  (AtomSymbol . TS.cons ':')


sym :: Text -> Grammar Position (Sexp :- t) t
sym s = atom >>> Flip (PartialIso
  (AtomSymbol s :-)
  (\(a :- t) ->
      case a of
        AtomSymbol s' | s == s' -> Right t
        other -> Left $ expected ("symbol " <> s) <> unexpected (ppBrief $ Atom other)))


kwd :: Text -> Grammar Position (Sexp :- t) t
kwd s =
  let k = TS.cons ':' s
  in atom >>> Flip (PartialIso
       (AtomSymbol k :-)
       (\(a :- t) ->
           case a of
             AtomSymbol s' | k == s' -> Right t
             other -> Left $ expected (ppKey k) <> unexpected (ppBrief $ Atom other)))

----------------------------------------------------------------------
-- Special

enum :: (Enum a, Bounded a, Eq a, Data a) => Grammar Position (Sexp :- t) (a :- t)
enum = coproduct $ map (\a -> sym (getEnumName a) >>> push a (== a)) [minBound .. maxBound]
  where
    getEnumName :: (Data a) => a -> Text
    getEnumName = TS.pack . lispifyName . showConstr . toConstr

    lispifyName :: String -> String
    lispifyName =
      intercalate "-" .
        map (map toLower) .
        split (dropBlanks . dropInitBlank . condense . keepDelimsL $ whenElt isUpper)
