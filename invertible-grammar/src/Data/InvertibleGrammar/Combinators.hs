{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.InvertibleGrammar.Combinators
  ( iso
  , osi
  , partialIso
  , partialOsi
  , push
  , pair
  , swap
  , cons
  , nil
  , insert
  , insertMay
  , toDefault
  , coproduct
  , onHead
  , onTail
  , traversed
  , flipped
  , sealed
  , coerced
  , annotated
  ) where

import Control.Category ((>>>))
import Data.Coerce
import Data.Maybe
import Data.Text (Text)
import Data.InvertibleGrammar.Base

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif


iso :: (a -> b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t


osi :: (b -> a) -> (a -> b) -> Grammar p (a :- t) (b :- t)
osi f' g' = Iso g f
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t


partialIso :: (a -> b) -> (b -> Either Mismatch a) -> Grammar p (a :- t) (b :- t)
partialIso f' g' = PartialIso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b


partialOsi :: (a -> Either Mismatch b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
partialOsi g' f' = Flip $ PartialIso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b


push :: a -> (a -> Bool) -> Grammar p t (a :- t)
push a p = PartialIso f g
  where
    f t = a :- t
    g (a' :- t)
      | p a' = Right t
      | otherwise = Left $ unexpected "pushed element"


pair :: Grammar p (b :- a :- t) ((a, b) :- t)
pair = Iso
  (\(b :- a :- t) -> (a, b) :- t)
  (\((a, b) :- t) -> b :- a :- t)


cons :: Grammar p ([a] :- a :- t) ([a] :- t)
cons = PartialIso
  (\(lst :- el :- t) -> (el:lst) :- t)
  (\(lst :- t) ->
      case lst of
        [] -> Left (expected "list element")
        (el:rest) -> Right (rest :- el :- t))


nil :: Grammar p t ([a] :- t)
nil = PartialIso
  (\t -> [] :- t)
  (\(lst :- t) ->
      case lst of
        [] -> Right t
        (_el:_rest) -> Left (expected "end of list"))


swap :: Grammar p (a :- b :- t) (b :- a :- t)
swap = Iso
  (\(a :- b :- t) -> (b :- a :- t))
  (\(b :- a :- t) -> (a :- b :- t))


insert :: (Eq k) => k -> Mismatch -> Grammar p (v :- [(k, v)] :- t) ([(k, v)] :- t)
insert k m = PartialIso
  (\(v :- alist :- t) -> ((k, v) : alist) :- t)
  (\(alist :- t) ->
     case popKey k alist of
       Nothing -> Left m
       Just (v, alist') -> Right (v :- alist' :- t))


insertMay :: (Eq k) => k -> Grammar p (Maybe v :- [(k, v)] :- t) ([(k, v)] :- t)
insertMay k = PartialIso
  (\(mv :- alist :- t) ->
      case mv of
        Just v -> ((k, v) : alist) :- t
        Nothing -> alist :- t)
  (\(alist :- t) ->
     case popKey k alist of
       Nothing -> Right (Nothing :- alist :- t)
       Just (v, alist') -> Right (Just v :- alist' :- t))


popKey :: forall k v. Eq k => k -> [(k, v)] -> Maybe (v, [(k, v)])
popKey k' = go []
  where
    go :: [(k, v)] -> [(k, v)] -> Maybe (v, [(k, v)])
    go acc (x@(k, v) : xs)
      | k == k' = Just (v, reverse acc ++ xs)
      | otherwise = go (x:acc) xs
    go _ [] = Nothing


toDefault :: (Eq a) => a -> Grammar p (Maybe a :- t) (a :- t)
toDefault def = iso
  (fromMaybe def)
  (\val -> if val == def then Nothing else Just val)


----------------------------------------------------------------------

coproduct :: [Grammar p a b] -> Grammar p a b
coproduct = foldl1 (<>)


onHead :: Grammar p a b -> Grammar p (a :- t) (b :- t)
onHead = OnHead


onTail :: Grammar p ta tb -> Grammar p (h :- ta) (h :- tb)
onTail = OnTail


traversed :: (Traversable f) => Grammar p a b -> Grammar p (f a) (f b)
traversed = Traverse


flipped :: Grammar p a b -> Grammar p b a
flipped = Flip


sealed :: (forall t. Grammar p (a :- t) (b :- t)) -> Grammar p a b
sealed g =
  Iso (:- undefined) (\(a :- _) -> a) >>>
  g >>>
  Iso (\(a :- _) -> a) (:- undefined)


coerced :: (Coercible a c, Coercible b d) => Grammar p (a :- t) (b :- t') -> Grammar p (c :- t) (d :- t')
coerced g = iso coerce coerce >>> g >>> iso coerce coerce


annotated :: Text -> Grammar p a b -> Grammar p a b
annotated =
  Annotate
