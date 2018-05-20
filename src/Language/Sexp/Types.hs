{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}

module Language.Sexp.Types
  ( Atom (..)
  , Sexp
  , pattern Atom
  , pattern Number
  , pattern Symbol
  , pattern String
  , pattern ParenList
  , pattern BracketList
  , pattern BraceList
  , pattern Quoted
  , BareSexp
  , Fix (..)
  , SexpF (..)
  , Compose (..)
  , Position (..)
  , dummyPos
  , LocatedBy (..)
  , location
  , extract
  , extractRecursive
  ) where

import Control.DeepSeq

import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Foldable (cata, Fix (..))
import Data.Bifunctor
import Data.Scientific
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (..), colon, (<>))
import GHC.Generics

----------------------------------------------------------------------
-- Positions

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

data LocatedBy a e = !a :< e
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Bifunctor LocatedBy where
  bimap f g (a :< e) = f a :< g e

instance (Eq p) => Eq1 (LocatedBy p) where
  liftEq eq (p :< a) (q :< b) = p == q && a `eq` b

location :: LocatedBy a e -> a
location (a :< _) = a

extract :: LocatedBy a e -> e
extract (_ :< e) = e

extractRecursive :: (Functor f) => Fix (Compose (LocatedBy p) f) -> Fix f
extractRecursive = cata (Fix . extract . getCompose)

----------------------------------------------------------------------
-- Sexp

data Atom
  = AtomNumber {-# UNPACK #-} !Scientific
  | AtomString {-# UNPACK #-} !Text
  | AtomSymbol {-# UNPACK #-} !Text
    deriving (Show, Eq, Ord, Generic)

data SexpF e
  = AtomF        !Atom
  | ParenListF   [e]
  | BracketListF [e]
  | BraceListF   [e]
  | QuotedF      e
    deriving (Functor, Foldable, Traversable, Generic)

instance Eq1 SexpF where
  liftEq eq = go
    where
      go (AtomF a) (AtomF b) = a == b
      go (ParenListF as) (ParenListF bs) = liftEq eq as bs
      go (BracketListF as) (BracketListF bs) = liftEq eq as bs
      go (BraceListF as) (BraceListF bs) = liftEq eq as bs
      go (QuotedF a) (QuotedF b) = a `eq` b
      go _ _ = False

instance NFData Atom
instance NFData Position

instance NFData (Fix SexpF) where
  rnf = cata alg
    where
      alg :: SexpF () -> ()
      alg = \case
        AtomF a -> rnf a
        ParenListF as -> rnf as
        BracketListF as -> rnf as
        BraceListF as -> rnf as
        QuotedF a -> rnf a

instance NFData (Fix (Compose (LocatedBy Position) SexpF)) where
  rnf = rnf . extractRecursive

type BareSexp = Fix SexpF

type Sexp = Fix (Compose (LocatedBy Position) SexpF)

pattern Atom :: Atom -> Sexp
pattern Atom a <- Fix (Compose ((_ :: Position) :< AtomF a))
  where Atom a =  Fix (Compose (dummyPos :< AtomF a))

pattern Number :: Scientific -> Sexp
pattern Number a <- Fix (Compose ((_ :: Position) :< AtomF (AtomNumber a)))
  where Number a =  Fix (Compose (dummyPos :< AtomF (AtomNumber a)))

pattern Symbol :: Text -> Sexp
pattern Symbol a <- Fix (Compose ((_ :: Position) :< AtomF (AtomSymbol a)))
  where Symbol a =  Fix (Compose (dummyPos :< AtomF (AtomSymbol a)))

pattern String :: Text -> Sexp
pattern String a <- Fix (Compose ((_ :: Position) :< AtomF (AtomString a)))
  where String a =  Fix (Compose (dummyPos :< AtomF (AtomString a)))

pattern ParenList :: [Sexp] -> Sexp
pattern ParenList ls <- Fix (Compose ((_ :: Position) :< ParenListF ls))
  where ParenList ls =  Fix (Compose (dummyPos :< ParenListF ls))

pattern BracketList :: [Sexp] -> Sexp
pattern BracketList ls <- Fix (Compose ((_ :: Position) :< BracketListF ls))
  where BracketList ls =  Fix (Compose (dummyPos :< BracketListF ls))

pattern BraceList :: [Sexp] -> Sexp
pattern BraceList ls <- Fix (Compose ((_ :: Position) :< BraceListF ls))
  where BraceList ls =  Fix (Compose (dummyPos :< BraceListF ls))

pattern Quoted :: Sexp -> Sexp
pattern Quoted s <- Fix (Compose ((_ :: Position) :< QuotedF s))
  where Quoted s =  Fix (Compose (dummyPos :< QuotedF s))
