{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}

module Language.SexpGrammar.Base
  ( atom
  , list
  , vect
  , el
  , rest
  , props
  , key
  , keyMay
  , (.:)
  , (.:?)
  , bool
  , real
  , double
  , int
  , integer
  , string
  , string'
  , symbol
  , symbol'
  , keyword
  , sym
  , kw
  , enum
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Category ((>>>))

import qualified Data.ByteString.Lazy as BS
import Data.Data
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Combinators
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Language.Sexp.Encode (encode)
import Language.Sexp.Types
import Language.Sexp.Utils (lispifyName)

ppBrief :: Sexp -> Text
ppBrief = TS.decodeUtf8 . BS.toStrict . \case
  atom@Atom{} ->
    encode atom
  other ->
    let pp = encode other
    in if BS.length pp > 25
       then BS.take 25 pp <> "..."
       else pp

----------------------------------------------------------------------

position :: Grammar Position (Sexp :- t) (Position :- Sexp :- t)
position = Iso
  (\(s :- t) -> getPos s :- s :- t)
  (\(_ :- s :- t) -> s :- t)


locate :: Grammar Position (Sexp :- t) (Sexp :- t)
locate =
  position >>>
  onHead Locate >>>
  Iso (\(_ :- t) -> t) (\t -> dummyPos :- t)


atom :: Grammar Position (Sexp :- t) (Atom :- t)
atom = locate >>> partialOsi
  (\case
      Atom _pos a -> Right a
      other -> Left (expected "atom" <> unexpected (ppBrief other)))
  (Atom dummyPos)


list_ :: Grammar p (Sexp :- t) ([Sexp] :- t)
list_ = partialOsi
  (\case
      List _pos a -> Right a
      other -> Left (expected "list" <> unexpected (ppBrief other)))
  (List dummyPos)


list :: Grammar Position ([Sexp] :- t) ([Sexp] :- t') -> Grammar Position (Sexp :- t) t'
list g = locate >>> list_ >>> Dive (g >>> Flip nil)


vector_ :: Grammar p (Sexp :- t) ([Sexp] :- t)
vector_ = partialOsi
  (\case
      Vector _pos a -> Right a
      other -> Left (expected "vector" <> unexpected (ppBrief other)))
  (Vector dummyPos)


vect :: Grammar Position ([Sexp] :- t) ([Sexp] :- t') -> Grammar Position (Sexp :- t) t'
vect g = locate >>> vector_ >>> Dive (g >>> Flip nil)


----------------------------------------------------------------------

el :: Grammar p (a :- t) t' -> Grammar p ([a] :- t) ([a] :- t')
el g = Flip cons >>> onTail g >>> Step


rest :: (forall t. Grammar p (a :- t) (b :- t)) -> Grammar p ([a] :- t) ([a] :- [b] :- t)
rest g = onHead (Traverse (sealed g >>> Step)) >>> Iso (\a -> [] :- a) (\(_ :- a) -> a)


----------------------------------------------------------------------

props_ :: Grammar p ([Sexp] :- t) ([Sexp] :- [(Kw, Sexp)] :- t)
props_ = Flip $ PartialIso
  (\(rest :- alist :- t) ->
      (concatMap (\(k, v) -> [Atom dummyPos (AtomKeyword k), v]) alist ++ rest) :- t)
  (\(lst :- t) ->
      let (rest, alist) = takePairs lst [] in
      Right (rest :- alist :- t))
  where
    takePairs :: [Sexp] -> [(Kw, Sexp)] -> ([Sexp], [(Kw, Sexp)])
    takePairs (Atom _ (AtomKeyword k) : v : rest) acc = takePairs rest ((k, v) : acc)
    takePairs other acc = (other, acc)


props :: Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- t') -> Grammar p ([Sexp] :- t) ([Sexp] :- t')
props g = Dive $ props_ >>> onTail g >>> swap >>> Flip nil


key :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- a :- t)
key k g = Flip (insert k) >>> Step >>> onHead (sealed g) >>> swap


keyMay :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- Maybe a :- t)
keyMay k g = Flip (insertMay k) >>> Step >>> onHead (Traverse (sealed g)) >>> swap


(.:) :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- a :- t)
(.:) = key


(.:?) :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- Maybe a :- t)
(.:?) = keyMay

----------------------------------------------------------------------
-- Atoms

bool :: Grammar Position (Sexp :- t) (Bool :- t)
bool = atom >>> partialOsi
  (\case
      AtomBool b -> Right b
      other -> Left (expected "bool" <> unexpected (ppBrief $ Atom dummyPos other)))
  AtomBool


integer :: Grammar Position (Sexp :- t) (Integer :- t)
integer = atom >>> partialOsi
  (\case
      AtomInt i -> Right i
      other -> Left (expected "int" <> unexpected (ppBrief $ Atom dummyPos other)))
  AtomInt


int :: Grammar Position (Sexp :- t) (Int :- t)
int = integer >>> iso fromIntegral fromIntegral


real :: Grammar Position (Sexp :- t) (Scientific :- t)
real = atom >>> partialOsi
  (\case
      AtomReal r -> Right r
      other -> Left (expected "real" <> unexpected (ppBrief $ Atom dummyPos other)))
  AtomReal


double :: Grammar Position (Sexp :- t) (Double :- t)
double = real >>> iso toRealFloat fromFloatDigits


string :: Grammar Position (Sexp :- t) (Text :- t)
string = atom >>> partialOsi
  (\case
      AtomString s -> Right s
      other -> Left (expected "string" <> unexpected (ppBrief $ Atom dummyPos other)))
  AtomString

string' :: Grammar Position (Sexp :- t) (String :- t)
string' = string >>> iso TS.unpack TS.pack


symbol :: Grammar Position (Sexp :- t) (Text :- t)
symbol = atom >>> partialOsi
  (\case
      AtomSymbol s -> Right s
      other -> Left (expected "symbol" <> unexpected (ppBrief $ Atom dummyPos other)))
  AtomSymbol


symbol' :: Grammar Position (Sexp :- t) (String :- t)
symbol' = symbol >>> iso TS.unpack TS.pack


keyword :: Grammar Position (Sexp :- t) (Kw :- t)
keyword = atom >>> partialOsi
  (\case
      AtomKeyword k -> Right k
      other -> Left (expected "keyword" <> unexpected (ppBrief $ Atom dummyPos other)))
  AtomKeyword


sym :: Text -> Grammar Position (Sexp :- t) t
sym s = atom >>> Flip (PartialIso
  (AtomSymbol s :-)
  (\(a :- t) ->
      case a of
        AtomSymbol s' | s == s' -> Right t
        other -> Left $ expected ("symbol " <> s) <> unexpected (ppBrief $ Atom dummyPos other)))


kw :: Kw -> Grammar Position (Sexp :- t) t
kw k = atom >>> Flip (PartialIso
  (AtomKeyword k :-)
  (\(a :- t) ->
      case a of
        AtomKeyword k' | k == k' -> Right t
        other -> Left $ expected ("keyword :" <> unKw k) <> unexpected (ppBrief $ Atom dummyPos other)))

----------------------------------------------------------------------
-- Special

enum :: (Enum a, Bounded a, Eq a, Data a) => Grammar Position (Sexp :- t) (a :- t)
enum = coproduct $ map (\a -> sym (getEnumName a) >>> push a (== a)) [minBound .. maxBound]
  where
    getEnumName :: (Data a) => a -> Text
    getEnumName = TS.pack . lispifyName . showConstr . toConstr
