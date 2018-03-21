{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.InvertibleGrammar
  ( Grammar (..)
  , (:-) (..)
  , iso
  , osi
  , partialIso
  , partialOsi
  , push
  , pushForget
  , forward
  , backward
  , GrammarError (..)
  , Mismatch
  , expected
  , unexpected
  ) where

import Prelude hiding ((.), id)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Category
import Control.Monad
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.InvertibleGrammar.Monad


data Grammar p a b where
  PartialIso :: String -> (a -> b) -> (b -> Either Mismatch a) -> Grammar p a b
  Iso        :: (a -> b) -> (b -> a) -> Grammar p a b
  Flip       :: Grammar p a b -> Grammar p b a
  (:.:)      :: Grammar p b c -> Grammar p a b -> Grammar p a c
  (:<>:)     :: Grammar p a b -> Grammar p a b -> Grammar p a b
  Over       :: (Traversable f) => Grammar p a b -> Grammar p (f a) (f b)
  Dive       :: Grammar p a b -> Grammar p a b
  Step       :: Grammar p a a
  Locate     :: Grammar p p p


instance Category (Grammar p) where
  id = Iso id id
  (.) x y = x :.: y

instance Semigroup (Grammar p a b) where
  (<>) = (:<>:)

data h :- t = h :- t deriving (Eq, Show, Functor, Foldable, Traversable)
infixr 5 :-


-- | Make a grammar from a total isomorphism on top element of stack
iso :: (a -> b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- | Make a grammar from a total isomorphism on top element of stack (flipped)
osi :: (b -> a) -> (a -> b) -> Grammar p (a :- t) (b :- t)
osi f' g' = Iso g f
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- | Make a grammar from a partial isomorphism which can fail during backward
-- run
partialIso :: String -> (a -> b) -> (b -> Either Mismatch a) -> Grammar p (a :- t) (b :- t)
partialIso prismName f' g' = PartialIso prismName f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b

-- | Make a grammar from a partial isomorphism which can fail during forward run
partialOsi :: String -> (b -> a) -> (a -> Either Mismatch b) -> Grammar p (a :- t) (b :- t)
partialOsi prismName f' g' = Flip $ PartialIso prismName f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b

-- | Unconditionally push given value on stack, i.e. it does not consume
-- anything on parsing. However such grammar expects the same value as given one
-- on the stack during backward run.
push :: (Eq a) => a -> Grammar p t (a :- t)
push a = PartialIso "push" f g
  where
    f t = a :- t
    g (a' :- t)
      | a == a' = Right t
      | otherwise = Left $ unexpected "pushed element"

-- | Same as 'push' except it does not check the value on stack during backward
-- run. Potentially unsafe as it \"forgets\" some data.
pushForget :: a -> Grammar p t (a :- t)
pushForget a = Iso f g
  where
    f t = a :- t
    g (_ :- t) = t


forward :: Grammar p a b -> a -> ContextError (Propagation p) (GrammarError p) b
forward (Iso f _)           = return . f
forward (PartialIso _ f _)  = return . f
forward (Flip g)            = backward g
forward (g :.: f)           = forward g <=< forward f
forward (f :<>: g)          = \x -> forward f x `mplus` forward g x
forward (Over g)            = traverse (forward g)
forward (Dive g)            = dive . forward g
forward Step                = \x -> step >> return x
forward Locate              = \x -> locate x >> return x
{-# INLINE forward #-}

backward :: Grammar p a b -> b -> ContextError (Propagation p) (GrammarError p) a
backward (Iso _ g)          = return . g
backward (PartialIso _ _ g) = either (\mis -> throwInContext (\ctx -> GrammarError ctx mis)) return . g
backward (Flip g)           = forward g
backward (g :.: f)          = backward g >=> backward f
backward (f :<>: g)         = \x -> backward f x `mplus` backward g x
backward (Over g)           = traverse (backward g)
backward (Dive g)           = dive . backward g
backward Step               = \x -> step >> return x
backward Locate             = \x -> locate x >> return x
{-# INLINE backward #-}
