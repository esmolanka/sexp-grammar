{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}

module Language.Sexp.Types
  ( Atom (..)
  , Prefix (..)
  , Fix (..)
  , SexpF (..)
  , Compose (..)
  , Position (..)
  , dummyPos
  , LocatedBy (..)
  , location
  , extract
  , stripLocation
  , addLocation
  ) where

import Control.DeepSeq

import Data.Bifunctor

import Data.Fix (Fix (..))
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Foldable (cata)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics
import Prettyprinter (Pretty (..), colon)

----------------------------------------------------------------------
-- Positions

-- | Position: file name, line number, column number
data Position =
  Position FilePath {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Ord, Eq, Generic)

dummyPos :: Position
dummyPos = Position "<no location information>" 1 0

instance Pretty Position where
  pretty (Position fn line col) =
    pretty fn <> colon <> pretty line <> colon <> pretty col

instance Show Position where
  show (Position fn line col) =
    fn ++ ":" ++ show line ++ ":" ++ show col

----------------------------------------------------------------------
-- Annotations

-- | Annotation functor for positions
data LocatedBy a e = !a :< e
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Bifunctor LocatedBy where
  bimap f g (a :< e) = f a :< g e

instance (Eq p) => Eq1 (LocatedBy p) where
  liftEq eq (p :< a) (q :< b) = p == q && a `eq` b

instance NFData p => NFData1 (LocatedBy p) where
  liftRnf f (p :< a) = rnf p `seq` f a

location :: LocatedBy a e -> a
location (a :< _) = a

extract :: LocatedBy a e -> e
extract (_ :< e) = e

stripLocation :: (Functor f) => Fix (Compose (LocatedBy p) f) -> Fix f
stripLocation = cata (Fix . extract . getCompose)

addLocation :: (Functor f) => p -> Fix f -> Fix (Compose (LocatedBy p) f)
addLocation p = cata (Fix . Compose . (p :<))

----------------------------------------------------------------------
-- Sexp

-- | S-expression atom type
data Atom
  = AtomNumber {-# UNPACK #-} !Scientific
  | AtomString {-# UNPACK #-} !Text
  | AtomSymbol {-# UNPACK #-} !Text
    deriving (Show, Eq, Ord, Generic)

-- | S-expression quotation type
data Prefix
  = Quote
  | Backtick
  | Comma
  | CommaAt
  | Hash
    deriving (Show, Eq, Ord, Generic)

instance NFData Prefix

-- | S-expression functor
data SexpF e
  = AtomF        !Atom
  | ParenListF   [e]
  | BracketListF [e]
  | BraceListF   [e]
  | ModifiedF    !Prefix e
    deriving (Functor, Foldable, Traversable, Generic)

instance Eq a => Eq (SexpF a) where
  (==) = liftEq (==)

instance Eq1 SexpF where
  liftEq eq = go
    where
      go (AtomF a) (AtomF b) = a == b
      go (ParenListF as) (ParenListF bs) = liftEq eq as bs
      go (BracketListF as) (BracketListF bs) = liftEq eq as bs
      go (BraceListF as) (BraceListF bs) = liftEq eq as bs
      go (ModifiedF q a) (ModifiedF p b) = q == p && a `eq` b
      go _ _ = False

instance NFData Atom

instance NFData Position

instance NFData1 SexpF where
  liftRnf f = \case
    AtomF a -> rnf a
    ParenListF as -> liftRnf f as
    BracketListF as -> liftRnf f as
    BraceListF as -> liftRnf f as
    ModifiedF q a -> rnf q `seq` f a

instance NFData1 (Compose (LocatedBy Position) SexpF) where
  liftRnf f (Compose (p :< a)) = rnf p `seq` liftRnf f a
