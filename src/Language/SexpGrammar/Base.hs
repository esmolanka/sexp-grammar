{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE LambdaCase            #-}

module Language.SexpGrammar.Base where

import Control.Category ((>>>))

import Data.Data
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.ByteString.Lazy as BS
import Data.Scientific
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Monad (ContextError, Propagation, GrammarError)

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

runParse :: Grammar Position (Sexp :- ()) (a :- ()) -> Sexp -> ContextError (Propagation Position) (GrammarError Position) a
runParse gram input =
  (\(x :- _) -> x) <$> forward gram (input :- ())

runGen :: Grammar Position (Sexp :- ()) (a :- ()) -> a -> ContextError (Propagation Position) (GrammarError Position) Sexp
runGen gram input =
  (\(x :- _) -> x) <$> backward gram (input :- ())

----------------------------------------------------------------------

position :: Grammar Position (Sexp :- t) (Position :- Sexp :- t)
position = Iso (\(s :- t) -> getPos s :- s :- t) (\(_ :- s :- t) -> s :- t)

locate :: Grammar Position (Sexp :- t) (Sexp :- t)
locate =
  position >>>
  overHead Locate >>>
  Iso (\(_ :- t) -> t) (\t -> dummyPos :- t)

atom :: Grammar Position (Sexp :- t) (Atom :- t)
atom = locate >>> partialOsi "Atom"
  (Atom dummyPos)
  (\case
      Atom _pos a -> Right a
      other -> Left (expected "atom" <> unexpected (ppBrief other)))

list_ :: Grammar p (Sexp :- t) ([Sexp] :- t)
list_ = partialOsi "List"
  (List dummyPos)
  (\case
      List _pos a -> Right a
      other -> Left (expected "list" <> unexpected (ppBrief other)))


list :: Grammar Position ([Sexp] :- t) ([Sexp] :- t') -> Grammar Position (Sexp :- t) t'
list g = locate >>> list_ >>> Dive (g >>> nil)


vector_ :: Grammar p (Sexp :- t) ([Sexp] :- t)
vector_ = partialOsi "Vector"
  (Vector dummyPos)
  (\case
      Vector _pos a -> Right a
      other -> Left (expected "vector" <> unexpected (ppBrief other)))


vect :: Grammar Position ([Sexp] :- t) ([Sexp] :- t') -> Grammar Position (Sexp :- t) t'
vect g = locate >>> vector_ >>> Dive (g >>> nil)


----------------------------------------------------------------------

el :: Grammar p (a :- t) t' -> Grammar p ([a] :- t) ([a] :- t')
el g = cons >>> swap >>> Over g >>> Step


rest :: (forall t. Grammar p (a :- t) (b :- t)) -> Grammar p ([a] :- t) ([a] :- [b] :- t)
rest g = overHead (Over (unTail g >>> Step)) >>> Iso (\a -> [] :- a) (\(_ :- a) -> a)


----------------------------------------------------------------------

props' :: Grammar p ([Sexp] :- t) ([Sexp] :- [(Kw, Sexp)] :- t)
props' = Flip $ PartialIso "properties"
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
props g = Dive $ props' >>> Over g >>> swap >>> nil


key :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- a :- t)
key k g = lkp k >>> Step >>> overHead (unTail g) >>> swap


keyMay :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- Maybe a :- t)
keyMay k g = lkpMay k >>> Step >>> overHead (Over (unTail g)) >>> swap


(.:) :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- a :- t)
(.:) = key


(.:?) :: Kw -> (forall t. Grammar p (Sexp :- t) (a :- t)) -> Grammar p ([(Kw, Sexp)] :- t) ([(Kw, Sexp)] :- Maybe a :- t)
(.:?) = keyMay

----------------------------------------------------------------------
-- Utils

pair :: Grammar p ((a, b) :- t) (b :- a :- t)
pair = Iso
  (\((a, b) :- t) -> b :- a :- t)
  (\(b :- a :- t) -> (a, b) :- t)


cons :: Grammar p ([a] :- t) (a :- [a] :- t)
cons = Flip $ PartialIso "element"
  (\(el :- lst :- t) -> (el:lst) :- t)
  (\(lst :- t) ->
      case lst of
        [] -> Left (expected "list element")
        (el:rest) -> Right (el :- rest :- t))


nil :: Grammar p ([a] :- t) t
nil = Flip $ PartialIso "end-of-list"
  (\t -> [] :- t)
  (\(lst :- t) ->
      case lst of
        [] -> Right t
        (_el:_rest) -> Left (expected "end-of-list"))


swap :: Grammar p (a :- b :- t) (b :- a :- t)
swap = Iso
  (\(a :- b :- t) -> (b :- a :- t))
  (\(b :- a :- t) -> (a :- b :- t))


dup :: (forall t. Grammar p (a :- t) (b :- t)) -> Grammar p (a :- t) (b :- a :- t)
dup g = Iso (\(s :- t) -> s :- s :- t) (\(_ :- s :- t) -> s :- t) >>> overHead (unTail g)


lkp :: (Eq k, Show k) => k -> Grammar p ([(k, v)] :- t) (v :- [(k, v)] :- t)
lkp k = Flip $ PartialIso "key"
  (\(v :- alist :- t) -> ((k, v) : alist) :- t)
  (\(alist :- t) ->
     case popKey k alist of
       Nothing -> Left (expected ("key " <> TS.pack (show k)))
       Just (v, alist') -> Right (v :- alist' :- t))


lkpMay :: (Eq k, Show k) => k -> Grammar p ([(k, v)] :- t) (Maybe v :- [(k, v)] :- t)
lkpMay k = Flip $ PartialIso "key"
  (\(mv :- alist :- t) ->
      case mv of
        Just v -> ((k, v) : alist) :- t
        Nothing -> alist :- t)
  (\(alist :- t) ->
     case popKey k alist of
       Nothing -> Right (Nothing :- alist :- t)
       Just (v, alist') -> Right (Just v :- alist' :- t))


popKey :: forall k v. Eq k => k -> [(k, v)] -> Maybe (v, [(k, v)])
popKey k' alist = go [] alist
  where
    go :: [(k, v)] -> [(k, v)] -> Maybe (v, [(k, v)])
    go acc (x@(k, v) : xs)
      | k == k' = Just (v, reverse acc ++ xs)
      | otherwise = go (x:acc) xs
    go _ [] = Nothing


over :: (Traversable f) => Grammar p a b -> Grammar p (f a) (f b)
over = Over


overHead :: Grammar p a b -> Grammar p (a :- t) (b :- t)
overHead g =
  Iso (\(a :- t) -> (t, a)) (\(t, a) -> (a :- t)) >>>
  Over g >>>
  Iso (\(t, a) -> (a :- t)) (\(a :- t) -> (t, a))


unTail :: (forall t. Grammar p (a :- t) (b :- t)) -> Grammar p a b
unTail g =
  Iso (\a -> (a :- undefined)) (\(a :- _) -> a) >>>
  g >>>
  Iso (\(a :- _) -> a) (\a -> (a :- undefined))


coproduct :: [Grammar p a b] -> Grammar p a b
coproduct = foldr1 (<>)


enum :: (Enum a, Bounded a, Eq a, Data a) => Grammar Position (Sexp :- t) (a :- t)
enum = coproduct $ map (\a -> sym (getEnumName a) >>> push a) [minBound .. maxBound]
  where
    getEnumName :: (Data a) => a -> Text
    getEnumName = TS.pack . lispifyName . showConstr . toConstr

toDefault :: (Eq a) => a -> Grammar p (Maybe a :- t) (a :- t)
toDefault def = iso
  (maybe def id)
  (\val -> if val == def then Nothing else Just val)


un :: Grammar p a b -> Grammar p b a
un = Flip

----------------------------------------------------------------------
-- Atoms

bool :: Grammar Position (Sexp :- t) (Bool :- t)
bool = atom >>> partialOsi "Bool"
  AtomBool
  (\case
      AtomBool b -> Right b
      other -> Left (expected "bool" <> unexpected (ppBrief $ Atom dummyPos other)))

integer :: Grammar Position (Sexp :- t) (Integer :- t)
integer = atom >>> partialOsi "Integer"
  AtomInt
  (\case
      AtomInt i -> Right i
      other -> Left (expected "int" <> unexpected (ppBrief $ Atom dummyPos other)))

int :: Grammar Position (Sexp :- t) (Int :- t)
int = integer >>> iso fromIntegral fromIntegral

real :: Grammar Position (Sexp :- t) (Scientific :- t)
real = atom >>> partialOsi "Scientific"
  AtomReal
  (\case
      AtomReal r -> Right r
      other -> Left (expected "real" <> unexpected (ppBrief $ Atom dummyPos other)))

double :: Grammar Position (Sexp :- t) (Double :- t)
double = real >>> iso toRealFloat fromFloatDigits

string :: Grammar Position (Sexp :- t) (Text :- t)
string = atom >>> partialOsi "Text"
  AtomString
  (\case
      AtomString s -> Right s
      other -> Left (expected "string" <> unexpected (ppBrief $ Atom dummyPos other)))

string' :: Grammar Position (Sexp :- t) (String :- t)
string' = string >>> iso TS.unpack TS.pack

symbol :: Grammar Position (Sexp :- t) (Text :- t)
symbol = atom >>> partialOsi "Symbol"
  AtomSymbol
  (\case
      AtomSymbol s -> Right s
      other -> Left (expected "symbol" <> unexpected (ppBrief $ Atom dummyPos other)))

symbol' :: Grammar Position (Sexp :- t) (String :- t)
symbol' = symbol >>> iso TS.unpack TS.pack

keyword :: Grammar Position (Sexp :- t) (Kw :- t)
keyword = atom >>> partialOsi "Keyword"
  AtomKeyword
  (\case
      AtomKeyword k -> Right k
      other -> Left (expected "keyword" <> unexpected (ppBrief $ Atom dummyPos other)))

sym :: Text -> Grammar Position (Sexp :- t) t
sym s = atom >>> Flip (PartialIso "Symbol"
  (AtomSymbol s :-)
  (\(a :- t) ->
      case a of
        AtomSymbol s' | s == s' -> Right t
        other -> Left $ expected ("symbol " <> s) <> unexpected (ppBrief $ Atom dummyPos other)))

kw :: Kw -> Grammar Position (Sexp :- t) t
kw k = atom >>> Flip (PartialIso "Keyword"
  (AtomKeyword k :-)
  (\(a :- t) ->
      case a of
        AtomKeyword k' | k == k' -> Right t
        other -> Left $ expected ("keyword :" <> unKw k) <> unexpected (ppBrief $ Atom dummyPos other)))
