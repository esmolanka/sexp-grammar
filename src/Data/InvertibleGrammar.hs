{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.InvertibleGrammar
  ( Grammar (..)
  , (:-) (..)
  , onHead
  , onTail
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
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable
import Data.Foldable
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.InvertibleGrammar.Monad
import qualified Debug.Trace

data h :- t = h :- t deriving (Eq, Show, Functor, Foldable, Traversable)
infixr 5 :-

instance Bifunctor (:-) where
  bimap f g (a :- b) = f a :- g b

instance Bifoldable (:-) where
  bifoldr f g x0 (a :- b) = a `f` (b `g` x0)

instance Bitraversable (:-) where
  bitraverse f g (a :- b) = (:-) <$> f a <*> g b

data Grammar p a b where
  Iso        :: (a -> b) -> (b -> a) -> Grammar p a b
  PartialIso :: (a -> b) -> (b -> Either Mismatch a) -> Grammar p a b
  Flip       :: Grammar p a b -> Grammar p b a
  (:.:)      :: Grammar p b c -> Grammar p a b -> Grammar p a c
  (:<>:)     :: Grammar p a b -> Grammar p a b -> Grammar p a b
  Traverse   :: (Traversable f) => Grammar p a b -> Grammar p (f a) (f b)
  OnHead     :: Grammar p a b -> Grammar p (a :- t) (b :- t)
  OnTail     :: Grammar p a b -> Grammar p (h :- a) (h :- b)
  Dive       :: Grammar p a b -> Grammar p a b
  Step       :: Grammar p a a
  Locate     :: Grammar p p p

trace :: String -> a -> a
trace = if False then Debug.Trace.trace else flip const


instance Category (Grammar p) where
  id                                              = Iso id id

  PartialIso f g        . Iso f' g'               = trace "p/i" $ PartialIso (f . f') (fmap g' . g)
  Iso f g               . PartialIso f' g'        = trace "i/p" $ PartialIso (f . f') (g' . g)

  Flip (PartialIso f g) . Iso f' g'               = trace "fp/i" $ Flip $ PartialIso (g' . f) (g . f')
  Iso f g               . Flip (PartialIso f' g') = trace "i/fp" $ Flip $ PartialIso (f' . g) (fmap f . g')

  PartialIso f g        . (Iso f' g'               :.: h) = trace "p/i2" $ PartialIso (f . f') (fmap g' . g) :.: h
  Iso f g               . (PartialIso f' g'        :.: h) = trace "i/p2" $ PartialIso (f . f') (g' . g) :.: h

  Flip (PartialIso f g) . (Iso f' g'               :.: h) = trace "fp/i2" $ Flip (PartialIso (g' . f) (g . f')) :.: h
  Iso f g               . (Flip (PartialIso f' g') :.: h) = trace "i/fp2" $ Flip (PartialIso (f' . g) (fmap f . g')) :.: h

  Flip g . Flip h                                 = trace "f/f" $ Flip (h . g)
  Iso f g . Iso f' g'                             = trace "i/i" $ Iso (f . f') (g' . g)

  (g :.: h)             . j                       = trace "assoc" $ g :.: (h . j)

  g                     . h                       = g :.: h


instance Semigroup (Grammar p a b) where
  (<>) = (:<>:)


onHead :: Grammar p a b -> Grammar p (a :- t) (b :- t)
onHead = OnHead

onTail :: Grammar p a b -> Grammar p (h :- a) (h :- b)
onTail = OnTail


-- | Make a grammar from a total isomorphism on top element of stack
iso :: (a -> b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- iso :: (a -> b) -> (b -> a) -> Grammar p (a :- t) (b :- t)
-- iso f g = onHead (Iso f g)


-- | Make a grammar from a total isomorphism on top element of stack (flipped)
osi :: (b -> a) -> (a -> b) -> Grammar p (a :- t) (b :- t)
osi f' g' = Iso g f
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- osi :: (b -> a) -> (a -> b) -> Grammar p (a :- t) (b :- t)
-- osi f g = onHead (Iso g f)

-- | Make a grammar from a partial isomorphism which can fail during backward
-- run
partialIso :: (a -> b) -> (b -> Either Mismatch a) -> Grammar p (a :- t) (b :- t)
partialIso f' g' = PartialIso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b

-- partialIso :: (a -> b) -> (b -> Either Mismatch a) -> Grammar p (a :- t) (b :- t)
-- partialIso f g = onHead (PartialIso f g)


-- | Make a grammar from a partial isomorphism which can fail during forward run
partialOsi :: (b -> a) -> (a -> Either Mismatch b) -> Grammar p (a :- t) (b :- t)
partialOsi f' g' = Flip $ PartialIso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b

-- partialOsi :: (b -> a) -> (a -> Either Mismatch b) -> Grammar p (a :- t) (b :- t)
-- partialOsi f g = onHead (PartialOsi g f)


-- | Unconditionally push given value on stack, i.e. it does not consume
-- anything on parsing. However such grammar expects the same value as given one
-- on the stack during backward run.
push :: (Eq a) => a -> Grammar p t (a :- t)
push a = PartialIso f g
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

----------------------------------------------------------------------

forward :: Grammar p a b -> a -> ContextError (Propagation p) (GrammarError p) b
forward (Iso f _)        = return . f
forward (PartialIso f _) = return . f
forward (Flip g)         = backward g
forward (g :.: f)        = forward g <=< forward f
forward (f :<>: g)       = \x -> forward f x `mplus` forward g x
forward (Traverse g)     = traverse (forward g)
forward (OnHead g)       = \(a :- b) -> (:- b) <$> forward g a
forward (OnTail g)       = \(a :- b) -> (a :-) <$> forward g b
forward (Dive g)         = dive . forward g
forward Step             = \x -> step >> return x
forward Locate           = \x -> locate x >> return x

backward :: Grammar p a b -> b -> ContextError (Propagation p) (GrammarError p) a
backward (Iso _ g)        = return . g
backward (PartialIso _ g) = either (\mis -> throwInContext (\ctx -> GrammarError ctx mis)) return . g
backward (Flip g)         = forward g
backward (g :.: f)        = backward g >=> backward f
backward (f :<>: g)       = \x -> backward f x `mplus` backward g x
backward (Traverse g)     = traverse (backward g)
backward (OnHead g)       = \(a :- b) -> (:- b) <$> backward g a
backward (OnTail g)       = \(a :- b) -> (a :-) <$> backward g b
backward (Dive g)         = dive . backward g
backward Step             = \x -> step >> return x
backward Locate           = \x -> locate x >> return x
