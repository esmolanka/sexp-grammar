
-- Adapted from Data.StackPrism.Generic originally written by Martijn van Steenbergen
-- Copyright (c) 2013, Martijn van Steenbergen

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DeriveGeneric #-}

module Data.StackPrism.ReverseGeneric (
    -- * Deriving stack prisms
    mkRevPrismList, StackPrisms, PrismList(..), MkPrismList, StackPrismLhs, MkStackPrism,
  ) where

import Data.StackPrism
import GHC.Generics

-- | Derive a list of stack prisms. For more information on the shape of a
-- 'PrismList', please see the documentation below.
mkRevPrismList :: (Generic a, MkPrismList (Rep a)) => StackPrisms a
mkRevPrismList = mkPrismList' to (Just . from)

-- | Convenient shorthand for a 'PrismList' indexed by a type and its generic
-- representation.
type StackPrisms a = PrismList (Rep a) a

-- | A data family that is indexed on the building blocks from representation
-- types from @GHC.Generics@. It builds up to a list of prisms, one for each
-- constructor in the generic representation. The list is wrapped in the unary
-- constructor @PrismList@. Within that constructor, the prisms are separated by
-- the right-associative binary infix constructor @:&@. Finally, the individual
-- prisms are wrapped in the unary constructor @P@.
--
-- As an example, here is how to define the prisms @nil@ and @cons@ for @[a]@,
-- which is an instance of @Generic@:
--
-- > nil  :: StackPrism              t  ([a] :- t)
-- > cons :: StackPrism (a :- [a] :- t) ([a] :- t)
-- > PrismList (P nil :& P cons) = mkPrismList :: StackPrisms [a]
data family PrismList (f :: * -> *) (a :: *)

class MkPrismList (f :: * -> *) where
  mkPrismList' :: (f p -> a) -> (a -> Maybe (f q)) -> PrismList f a

data instance PrismList (M1 D c f) a = PrismList (PrismList f a)

instance MkPrismList f => MkPrismList (M1 D c f) where
  mkPrismList' f' g' = PrismList (mkPrismList' (f' . M1) (fmap unM1 . g'))

infixr :&
data instance PrismList (f :+: g) a = PrismList f a :& PrismList g a

instance (MkPrismList f, MkPrismList g) => MkPrismList (f :+: g) where
  mkPrismList' f' g' = f f' g' :& g f' g'
    where
      f :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> PrismList f a
      f _f' _g' = mkPrismList' (\fp -> _f' (L1 fp)) (matchL _g')
      g :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> PrismList g a
      g _f' _g' = mkPrismList' (\gp -> _f' (R1 gp)) (matchR _g')

      matchL :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (f q)
      matchL _g' a = case _g' a of
        Just (L1 f'') -> Just f''
        _ -> Nothing

      matchR :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (g q)
      matchR _g' a = case _g' a of
        Just (R1 g'') -> Just g''
        _ -> Nothing

data instance PrismList (M1 C c f) a = P (forall t. StackPrism (StackPrismLhs f t) (a :- t))

instance MkStackPrism f => MkPrismList (M1 C c f) where
  mkPrismList' f' g' = P (stackPrism (f f') (g g'))
    where
      f :: forall a p t. (M1 C c f p -> a) -> StackPrismLhs f t -> a :- t
      f _f' lhs = mapHead (_f' . M1) (mkR lhs)
      g :: forall a p t. (a -> Maybe (M1 C c f p)) -> (a :- t) -> Maybe (StackPrismLhs f t)
      g _g' (a :- t) = fmap (mkL . (:- t) . unM1) (_g' a)

-- Deriving types and conversions for single constructors

type family StackPrismLhs (f :: * -> *) (t :: *) :: *

class MkStackPrism (f :: * -> *) where
  mkR :: forall p t. StackPrismLhs f t -> (f p :- t)
  mkL :: forall p t. (f p :- t) -> StackPrismLhs f t

type instance StackPrismLhs U1 t = t
instance MkStackPrism U1 where
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

type instance StackPrismLhs (K1 i a) t = a :- t
instance MkStackPrism (K1 i a) where
  mkR (h :- t) = K1 h :- t
  mkL (K1 h :- t) = h :- t

type instance StackPrismLhs (M1 i c f) t = StackPrismLhs f t
instance MkStackPrism f => MkStackPrism (M1 i c f) where
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

type instance StackPrismLhs (f :*: g) t = StackPrismLhs g (StackPrismLhs f t)
instance (MkStackPrism f, MkStackPrism g) => MkStackPrism (f :*: g) where
  mkR t = (hg :*: hf) :- tg
    where
      hf :- tf = mkR t
      hg :- tg = mkR tf
  mkL ((hf :*: hg) :- t) = mkL (hg :- mkL (hf :- t))

mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
