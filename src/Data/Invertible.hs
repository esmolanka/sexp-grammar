{-# LANGUAGE TypeFamilies #-}

module Data.Invertible where

class Invertible c where
  type InvertibleCtx c :: * -> *
  applyForward  :: c a b -> a -> InvertibleCtx c b
  applyBackward :: c a b -> b -> InvertibleCtx c a
