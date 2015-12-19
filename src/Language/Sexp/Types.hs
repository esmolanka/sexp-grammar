{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Sexp.Types where

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

newtype Fix f = Fix (f (Fix f))

data SexpF r
  = Atom Atom
  | List [r]
  | Vector [r]
  | Nil
    deriving (Functor)

type Sexp = Fix SexpF

instance Show (Fix SexpF) where
  show (Fix s) = show s

instance (Show f) => Show (SexpF f) where
  show (Atom a) = show a
  show (List ls) = "List " ++ show ls
  show (Vector ls) = "Vector " ++ show ls
  show Nil = "Nil"

