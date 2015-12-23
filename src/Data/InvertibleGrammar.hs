{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Data.InvertibleGrammar
  ( Grammar (..)
  , isoToGrammar
  , InvertibleGrammar(..)
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Data.Semigroup

import Data.PartialIso

data Grammar g t t' where
  LMap   :: Iso b a -> Grammar g a t -> Grammar g b t
  RMap   :: Iso a b -> Grammar g t a -> Grammar g t b
  Id     :: Grammar g t t
  (:.:)  :: Grammar g t1 t'' -> Grammar g t t1 -> Grammar g t t''
  -- Empty  :: Grammar g t t'
  (:<>:) :: Grammar g t t' -> Grammar g t t' -> Grammar g t t'
  -- Many   :: Gramar g (a :- t) (b :- t) -> Grammar g ([a] :- t) ([b] :- t)
  Many   :: Grammar g t t -> Grammar g t t
  Gram   :: g a b -> Grammar g a b

instance Category (Grammar c) where
  id = Id
  (.) Id y  = y
  (.) x  Id = x
  (.) x  y  = x :.: y

instance Semigroup (Grammar c t1 t2) where
  (<>) = (:<>:)

isoToGrammar :: Iso a b -> Grammar g a b
isoToGrammar iso = iso `RMap` id

-- instance Monoid (Grammar c t1 t2) where
--   mempty = Empty
--   mappend Empty y     = y
--   mappend x     Empty = x
--   mappend x     y     = x :<>: y

class InvertibleGrammar m g where
  parseWithGrammar :: g a b -> a -> m b
  -- printWithGrammar :: g a b -> b -> m a

instance (Monad m, MonadPlus m, InvertibleGrammar m g) => InvertibleGrammar m (Grammar g) where
  parseWithGrammar (LMap iso g) = parseWithGrammar g <<< applyForward iso
  parseWithGrammar (RMap iso g) = fmap (applyForward iso) . parseWithGrammar g
  parseWithGrammar Id           = return
  parseWithGrammar (g :.: f)    = parseWithGrammar g <=< parseWithGrammar f
  -- applyForward Empty        = applyForward g <=< applyForward f
  parseWithGrammar (f :<>: g)   = \x -> parseWithGrammar f x `mplus` parseWithGrammar g x
  parseWithGrammar (Many g)     = go
    where
      go x = (parseWithGrammar g x >>= go) `mplus` return x
  parseWithGrammar (Gram g)     = parseWithGrammar g
