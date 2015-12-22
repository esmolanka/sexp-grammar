{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.InvertibleGrammar
  ( Grammar (..)
  , isoToGrammar
  ) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Monad
import Data.Semigroup

import Data.Invertible
import Data.PartialIso

data Grammar m g t t' where
  LMap   :: Iso m b a -> Grammar m g a t -> Grammar m g b t
  RMap   :: Iso m a b -> Grammar m g t a -> Grammar m g t b
  Id     :: Grammar m g t t
  (:.:)  :: Grammar m g t1 t'' -> Grammar m g t t1 -> Grammar m g t t''
  -- Empty  :: Grammar m g t t'
  (:<>:) :: Grammar m g t t' -> Grammar m g t t' -> Grammar m g t t'
  -- Many   :: Gramar g (a :- t) (b :- t) -> Grammar g ([a] :- t) ([b] :- t)
  Many   :: Grammar m g t t -> Grammar m g t t
  Gram   :: g a b -> Grammar m g a b

instance Category (Grammar m c) where
  id = Id
  (.) Id y  = y
  (.) x  Id = x
  (.) x  y  = x :.: y

instance Semigroup (Grammar m c t1 t2) where
  (<>) = (:<>:)

isoToGrammar :: Iso m a b -> Grammar m g a b
isoToGrammar iso = iso `RMap` id

-- instance Monoid (Grammar m c t1 t2) where
--   mempty = Empty
--   mappend Empty y     = y
--   mappend x     Empty = x
--   mappend x     y     = x :<>: y

-- class InvertibleGrammar m g where
--   parseWithGrammar :: g a b -> a -> m b
--   -- printWithGrammar :: g a b -> b -> m a

instance
  ( Monad m
  , Alternative m
  , Invertible g
  , InvertibleCtx (Grammar m g) ~ InvertibleCtx g
  ) => Invertible (Grammar m g) where
  type InvertibleCtx (Grammar m g) = m
  applyForward (LMap iso g) = applyForward g <=< applyForward iso
  applyForward (RMap iso g) = applyForward iso <=< applyForward g
  applyForward Id           = return
  applyForward (g :.: f)    = applyForward g <=< applyForward f
  -- applyForward Empty        = applyForward g <=< applyForward f
  applyForward (f :<>: g)   = \x -> applyForward f x <|> applyForward g x
  applyForward (Many g)     = go
    where
      go x = (applyForward g x >>= go) <|> return x
  applyForward (Gram g)     = applyForward g

  applyBackward = undefined
