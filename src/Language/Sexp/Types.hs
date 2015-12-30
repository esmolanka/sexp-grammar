{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sexp.Types
  ( Atom (..)
  , Kw (..)
  , SexpF (..)
  , Sexp
  , Position (..)
  ) where

import Control.Comonad.Cofree
import Data.Scientific
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text (Pretty (..), int, colon, (<>))

data Position =
  Position !Int !Int
  deriving (Show, Ord, Eq)

instance Pretty Position where
  pretty (Position line col) = int line <> colon <> int col

newtype Kw = Kw { unKw :: Text }
  deriving (Show, Eq, Ord)

data Atom
  = AtomBool Bool
  | AtomInt Integer
  | AtomReal Scientific
  | AtomString Text
  | AtomSymbol Text
  | AtomKeyword Kw
    deriving (Show, Eq, Ord)

data SexpF r
  = Atom Atom
  | Quoted r
  | Vector [r]
  | List [r]
    deriving (Eq, Ord, Functor, Foldable, Traversable)

type Sexp = Cofree SexpF Position

instance (Show f) => Show (SexpF f) where
  show (Atom a) = show a
  show (List ls) = "List " ++ show ls
  show (Vector ls) = "Vector " ++ show ls
  show (Quoted a) = "Quoted (" ++ show a ++ ")"
