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
import Data.Text.Prettyprint.Doc (Pretty (..), colon, (<>))

-- | File position
data Position =
  Position !FilePath {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Ord, Eq)

dummyPos :: Position
dummyPos = Position "<no location information>" 1 0

instance Pretty Position where
  pretty (Position fn line col) =
    pretty fn <> colon <> pretty line <> colon <> pretty col

-- | Keyword newtype wrapper to distinguish keywords from symbols
newtype Kw = Kw { unKw :: Text }
  deriving (Show, Eq, Ord)

-- | Sexp atom types
data Atom
  = AtomBool Bool
  | AtomInt Integer
  | AtomReal Scientific
  | AtomString Text
  | AtomSymbol Text
  | AtomKeyword Kw
    deriving (Show, Eq, Ord)

-- | Sexp ADT
data Sexp
  = Atom   {-# UNPACK #-} !Position !Atom
  | List   {-# UNPACK #-} !Position [Sexp]
  | Vector {-# UNPACK #-} !Position [Sexp]
  | Quoted {-# UNPACK #-} !Position Sexp
    deriving (Show, Eq, Ord)

-- | Get position of Sexp element
getPos :: Sexp -> Position
getPos (Atom p _)   = p
getPos (Quoted p _) = p
getPos (Vector p _) = p
getPos (List p _)   = p
{-# INLINE getPos #-}
