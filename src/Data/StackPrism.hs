
-- Module originally written by Martijn van Steenbergen
-- Copyright (c) 2013, Martijn van Steenbergen

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.StackPrism (

  -- * Stack prisms
  StackPrism, stackPrism, forward, backward,
  (:-)(..)

  ) where


import Control.Applicative
import Data.Profunctor (Choice(..))
import Data.Profunctor.Unsafe
import Data.Functor.Identity
import Data.Monoid (First(..))
import Data.Tagged

-- | A stack prism is a bidirectional isomorphism that is partial in the backward direction.
-- These prisms are compatible with the @lens@ library.
--
-- Stack prisms can express constructor-deconstructor pairs. For example:
--
-- > nil :: StackPrism t ([a] :- t)
-- > nil = stackPrism f g
-- >   where
-- >     f        t  = [] :- t
-- >     g ([] :- t) = Just t
-- >     g _         = Nothing
-- >
-- > cons :: StackPrism (a :- [a] :- t) ([a] :- t)
-- > cons = stackPrism f g
-- >   where
-- >     f (x :- xs  :- t) = (x : xs) :- t
-- >     g ((x : xs) :- t) = Just (x :- xs :- t)
-- >     g _               = Nothing
--
-- Here ':-' can be read as \'cons\', forming a stack of values. For example,
-- @nil@ pushes @[]@ onto the stack; or, in the backward direction, tries to
-- remove @[]@ from the stack. @cons@ takes a head @x@ and tail @xs@ from the
-- stack and pushes @x : xs@ onto the stack, or, in the backward direction,
-- tries to take @x : xs@ from the stack and replaces it with its two
-- individual components.
--
-- Every constructor has its own stack prism version. You don't have to write
-- them by hand; you can automatically generate them, either using Template
-- Haskell (see module "Data.StackPrism.TH") or using GHC generic programming
-- (see module "Data.StackPrism.Generic").
type StackPrism a b = forall p f. (Choice p, Applicative f) => p a (f a) -> p b (f b)

-- | Construct a prism.
stackPrism :: (a -> b) -> (b -> Maybe a) -> StackPrism a b
stackPrism f g = dimap (\b -> maybe (Left b) Right (g b)) (either pure (fmap f)) . right'

-- | Apply a prism in forward direction.
forward :: StackPrism a b -> a -> b
forward l = runIdentity #. unTagged #. l .# Tagged .# Identity

-- | Apply a prism in backward direction.
backward :: StackPrism a b -> b -> Maybe a
backward l = getFirst #. getConst #. l (Const #. First #. Just)


-- | Heterogenous stack with a head and a tail. Or: an infix way to write @(,)@.
data h :- t = h :- t
  deriving (Eq, Show, Functor)
infixr 5 :-
