{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
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
  , InvertibleGrammar(..)
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
import Data.Semigroup
import Data.InvertibleGrammar.Monad

data Grammar g t t' where
  -- Partial isomorphism
  PartialIso :: String -> (a -> b) -> (b -> Either Mismatch a) -> Grammar g a b

  -- Total isomorphism
  Iso :: (a -> b) -> (b -> a) -> Grammar g a b

  -- Run a grammar in the opposite direction
  Flip :: Grammar g a b -> Grammar g b a

  -- Grammar composition
  (:.:) :: Grammar g b c -> Grammar g a b -> Grammar g a c

  -- Grammar alternation
  (:<>:) :: Grammar g a b -> Grammar g a b -> Grammar g a b

  -- Embed a subgrammar
  Inject :: g a b -> Grammar g a b

instance Category (Grammar c) where
  id = Iso id id
  (.) x y = x :.: y

instance Semigroup (Grammar c t1 t2) where
  (<>) = (:<>:)

data h :- t = h :- t deriving (Eq, Show, Functor)
infixr 5 :-

-- | Make a grammar from a total isomorphism on top element of stack
iso :: (a -> b) -> (b -> a) -> Grammar g (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- | Make a grammar from a total isomorphism on top element of stack (flipped)
osi :: (b -> a) -> (a -> b) -> Grammar g (a :- t) (b :- t)
osi f' g' = Iso g f
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- | Make a grammar from a partial isomorphism which can fail during backward
-- run
partialIso :: String -> (a -> b) -> (b -> Either Mismatch a) -> Grammar g (a :- t) (b :- t)
partialIso prismName f' g' = PartialIso prismName f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b

-- | Make a grammar from a partial isomorphism which can fail during forward run
partialOsi :: String -> (b -> a) -> (a -> Either Mismatch b) -> Grammar g (a :- t) (b :- t)
partialOsi prismName f' g' = Flip $ PartialIso prismName f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = (:- t) <$> g' b

-- | Unconditionally push given value on stack, i.e. it does not consume
-- anything on parsing. However such grammar expects the same value as given one
-- on the stack during backward run.
push :: (Eq a) => a -> Grammar g t (a :- t)
push a = PartialIso "push" f g
  where
    f t = a :- t
    g (a' :- t)
      | a == a' = Right t
      | otherwise = Left $ unexpected "pushed element"

-- | Same as 'push' except it does not check the value on stack during backward
-- run. Potentially unsafe as it \"forgets\" some data.
pushForget :: a -> Grammar g t (a :- t)
pushForget a = Iso f g
  where
    f t = a :- t
    g (_ :- t) = t

class InvertibleGrammar m g where
  forward  :: g a b -> (a -> m b)
  backward :: g a b -> (b -> m a)

instance
  ( Monad m
  , MonadPlus m
  , MonadContextError (Propagation p) (GrammarError p) m
  , InvertibleGrammar m g
  ) => InvertibleGrammar m (Grammar g) where
  forward (Iso f _)           = return . f
  forward (PartialIso _ f _)  = return . f
  forward (Flip g)            = backward g
  forward (g :.: f)           = forward g <=< forward f
  forward (f :<>: g)          = \x -> forward f x `mplus` forward g x
  forward (Inject g)          = forward g
  {-# INLINE forward #-}

  backward (Iso _ g)          = return . g
  backward (PartialIso _ _ g) = either (\mis -> throwInContext (\ctx -> GrammarError ctx mis)) return . g
  backward (Flip g)           = forward g
  backward (g :.: f)          = backward g >=> backward f
  backward (f :<>: g)         = \x -> backward f x `mplus` backward g x
  backward (Inject g)         = backward g
  {-# INLINE backward #-}
