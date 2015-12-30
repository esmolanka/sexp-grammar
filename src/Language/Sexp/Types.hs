{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sexp.Types
  ( Atom (..)
  , Kw (..)
  , Sexp (..)
  , Position (..)
  , dummyPos
  , getPos
  ) where

import Data.Scientific
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text (Pretty (..), int, colon, (<>))

data Position =
  Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Ord, Eq)

dummyPos :: Position
dummyPos = Position 0 0

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

data Sexp
  = Atom   {-# UNPACK #-} !Position !Atom
  | List   {-# UNPACK #-} !Position [Sexp]
  | Vector {-# UNPACK #-} !Position [Sexp]
  | Quoted {-# UNPACK #-} !Position Sexp
    deriving (Show, Eq, Ord)

{-# INLINE getPos #-}
getPos :: Sexp -> Position
getPos (Atom p _) = p
getPos (Quoted p _) = p
getPos (Vector p _) = p
getPos (List p _) = p
