{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sexp.Types
  ( Atom (..)
  , SexpF (..)
  , Sexp
  ) where

import Data.Functor.Foldable (Fix (..))
import Data.Scientific
import Data.Text (Text)

data Atom
  = AtomBool Bool
  | AtomInt Integer
  | AtomReal Scientific
  | AtomString Text
  | AtomSymbol Text
  | AtomKeyword Text
    deriving (Show, Eq, Ord)

data SexpF r
  = Atom Atom
  | Quoted r
  | Vector [r]
  | List [r]
    deriving (Eq, Ord, Functor, Foldable, Traversable)

type Sexp = Fix SexpF

instance (Show f) => Show (SexpF f) where
  show (Atom a) = show a
  show (List ls) = "List " ++ show ls
  show (Vector ls) = "Vector " ++ show ls
  show (Quoted a) = "Quoted (" ++ show a ++ ")"
