{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
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
import Data.Void
import Data.Text (Text)
import Data.InvertibleGrammar.Base

-- | Isomorphism on the stack head.
iso :: (a -> b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t


-- | Flipped isomorphism on the stack head.
osi :: (b -> a) -> (a -> b) -> Grammar p (a :- t) (b :- t)
osi f' g' = Iso g f
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- | Partial isomorphism (for backward run) on the stack head.
partialIso :: (a -> b) -> (b -> Either Mismatch a) -> Grammar p (a :- t) (b :- t)
partialIso f' g' = PartialIso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b


-- | Partial isomorphism (for forward run) on the stack head.
partialOsi :: (a -> Either Mismatch b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
partialOsi g' f' = Flip $ PartialIso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b


-- | Push an element to the stack on forward run, check if the element
-- satisfies predicate, otherwise report a mismatch.
push :: a -> (a -> Bool) -> (a -> Mismatch) -> Grammar p t (a :- t)
push a p e = PartialIso f g
  where
    f t = a :- t
    g (a' :- t)
      | p a' = Right t
      | otherwise = Left $ e a'


-- | 2-tuple grammar. Construct on forward run, deconstruct on
-- backward run.
pair :: Grammar p (b :- a :- t) ((a, b) :- t)
pair = Iso
  (\(b :- a :- t) -> (a, b) :- t)
  (\((a, b) :- t) -> b :- a :- t)


-- | List cons-cell grammar. Construct on forward run, deconstruct on
-- backward run.
cons :: Grammar p ([a] :- a :- t) ([a] :- t)
cons = PartialIso
  (\(lst :- el :- t) -> (el:lst) :- t)
  (\(lst :- t) ->
      case lst of
        [] -> Left (expected "list element")
        (el:rest) -> Right (rest :- el :- t))


-- | Empty list grammar. Construct empty list on forward run, check if
-- list is empty on backward run.
nil :: Grammar p t ([a] :- t)
nil = PartialIso
  (\t -> [] :- t)
  (\(lst :- t) ->
      case lst of
        [] -> Right t
        (_el:_rest) -> Left (expected "end of list"))


-- | Swap two topmost stack elements.
swap :: Grammar p (a :- b :- t) (b :- a :- t)
swap = Iso
  (\(a :- b :- t) -> (b :- a :- t))
  (\(b :- a :- t) -> (a :- b :- t))


-- | Assoc-list element grammar. Inserts an element (with static key)
-- on forward run, look up an element on backward run.
insert :: (Eq k) => k -> Mismatch -> Grammar p (v :- [(k, v)] :- t) ([(k, v)] :- t)
insert k m = PartialIso
  (\(v :- alist :- t) -> ((k, v) : alist) :- t)
  (\(alist :- t) ->
     case popKey k alist of
       Nothing -> Left m
       Just (v, alist') -> Right (v :- alist' :- t))


-- | Optional assoc-list element grammar. Like 'insert', but does not
-- report a mismatch on backward run. Instead takes and produces a
-- Maybe-value.
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


-- | Default value grammar. Replaces 'Nothing' with a default value on
-- forward run, an replaces a default value with 'Nothing' on backward
-- run.
toDefault :: (Eq a) => a -> Grammar p (Maybe a :- t) (a :- t)
toDefault def = iso
  (fromMaybe def)
  (\val -> if val == def then Nothing else Just val)


-- | Run a grammar operating on the stack head in a context where
-- there is no stack.
sealed :: Grammar p (a :- Void) (b :- Void) -> Grammar p a b
sealed g =
  Iso (:- error "void") (\(a :- _) -> a) >>>
  g >>>
  Iso (\(a :- _) -> a) (:- error "void")


-- | Focus a given grammar to the stack head.
onHead :: Grammar p a b -> Grammar p (a :- t) (b :- t)
onHead = OnHead


-- | Focus a given grammar to the stack tail.
onTail :: Grammar p ta tb -> Grammar p (h :- ta) (h :- tb)
onTail = OnTail


-- | Traverse a structure with a given grammar.
traversed :: (Traversable f) => Grammar p a b -> Grammar p (f a) (f b)
traversed = Traverse


-- | Run a grammar with inputs and outputs flipped.
flipped :: Grammar p a b -> Grammar p b a
flipped = Flip


-- | Run a grammar with an annotation.
annotated :: Text -> Grammar p a b -> Grammar p a b
annotated = Annotate


-- | Run a grammar with the stack heads coerced to other ('Coercible')
-- types.
coerced
  :: (Coercible a c, Coercible b d) =>
     Grammar p (a :- t) (b :- t')
  -> Grammar p (c :- t) (d :- t')
coerced g = iso coerce coerce >>> g >>> iso coerce coerce


-- | Join alternative grammars in parallel.
coproduct :: [Grammar p a b] -> Grammar p a b
coproduct = foldl1 (<>)
