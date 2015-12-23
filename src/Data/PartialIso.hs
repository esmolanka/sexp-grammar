{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Data.PartialIso where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Except

infixr 5 :-
data a :- b = a :- b
  deriving (Functor)

data Iso a b = Iso (a -> b) (b -> Maybe a)

instance Category Iso where
  id = Iso id return
  (.) (Iso f g) (Iso f' g') = Iso (f' >>> f) (g >=> g')

applyForward :: Iso a b -> a -> b
applyForward  (Iso f _) = f

applyBackward :: Iso a b -> b -> Maybe a
applyBackward (Iso _ g) = g

isoFirst :: Iso a b -> Iso (a, t) (b, t)
isoFirst (Iso f g) = Iso f' g'
  where
    f' (x, t) = (f x, t)
    g' (y, t) = (, t) <$> g y

isoOnPairFst :: Iso a b -> Iso (a :- t) (b :- t)
isoOnPairFst (Iso f g) = Iso f' g'
  where
    f' (x :- t) = (f x :- t)
    g' (y :- t) = (:- t) <$> g y

isoCons :: Iso (a :- [a] :- t) ([a] :- t)
isoCons = Iso f g
  where
    f (x :- xs :- t)  = (x : xs) :- t
    g ((x : xs) :- t) = Just $ x :- xs :- t
    g ([]       :- _) = Nothing -- throwError "isoCons: expected non-empty list"

isoNil :: Iso t ([a] :- t)
isoNil = Iso f g
  where
    f t = [] :- t
    g ([] :- t) = Just t
    g (_  :- _) = Nothing -- throwError "isoCons: expected empty list"

-- swapIso :: Iso a b -> Iso b a
-- swapIso (Iso f g) = Iso g f
