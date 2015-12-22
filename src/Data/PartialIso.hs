{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Data.PartialIso where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Except

import Data.Invertible

infixr 5 :-
data a :- b = a :- b
  deriving (Functor)

data Iso m a b = Iso (a -> m b) (b -> m a)

instance (Monad m) => Category (Iso m) where
  id = Iso return return
  (.) (Iso f g) (Iso f' g') = Iso (f' >=> f) (g >=> g')

instance Invertible (Iso m) where
  type InvertibleCtx (Iso m) = m
  applyForward  (Iso f _) = f
  applyBackward (Iso _ g) = g

isoFirst :: (Applicative m) => Iso m a b -> Iso m (a, t) (b, t)
isoFirst (Iso f g) = Iso f' g'
  where
    f' (x, t) = (, t) <$> f x
    g' (y, t) = (, t) <$> g y

isoOnPairFst :: (Applicative m) => Iso m a b -> Iso m (a :- t) (b :- t)
isoOnPairFst (Iso f g) = Iso f' g'
  where
    f' (x :- t) = (:- t) <$> f x
    g' (y :- t) = (:- t) <$> g y

isoCons :: (MonadError String m) => Iso m (a :- [a] :- t) ([a] :- t)
isoCons = Iso f g
  where
    f (x :- xs :- t) = return $ (x : xs) :- t
    g ((x : xs) :- t) = return $ x :- xs :- t
    g ([]       :- _) = throwError "isoCons: expected non-empty list"

isoNil :: (MonadError String m) => Iso m t ([a] :- t)
isoNil = Iso f g
  where
    f t = return $ [] :- t
    g ([] :- t) = return t
    g (_  :- _) = throwError "isoCons: expected empty list"

swapIso :: Iso m a b -> Iso m b a
swapIso (Iso f g) = Iso g f
