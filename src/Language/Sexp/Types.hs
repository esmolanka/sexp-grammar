{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sexp.Types where

import Data.Functor.Foldable (Fix (..))
import qualified Data.List.NonEmpty as NE
import Data.Scientific
import Data.Text (Text)

data Atom
  = AtomBool Bool
  | AtomInt Integer
  | AtomReal Scientific
  | AtomString Text
  | AtomSymbol Text
  | AtomKeyword Text
    deriving (Show)

data SexpF r
  = Atom Atom
  | Quoted r
  | Vector [r]
  | List (NE.NonEmpty r) r
  | Nil
    deriving (Functor, Foldable, Traversable)

type Sexp = Fix SexpF

instance (Show f) => Show (SexpF f) where
  show (Atom a) = show a
  show (List ls other)     = "List " ++ show ls ++ " " ++ show other
  show Nil = "Nil"
  show (Vector ls) = "Vector " ++ show ls
  show (Quoted a) = "Quoted (" ++ show a ++ ")"

