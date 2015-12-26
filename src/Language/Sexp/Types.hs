{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Language.Sexp.Types
  ( Atom (..)
  , SexpF (..)
  , Sexp
  , boolSexpP
  , intSexpP
  , realSexpP
  , stringSexpP
  , symbolSexpP
  , keywordSexpP
  ) where

import Data.Functor.Foldable (Fix (..))
import Data.Scientific
import Data.Text (Text)

import Data.StackPrism.Extra
import Data.StackPrism.TH

data Atom
  = AtomBool Bool
  | AtomInt Integer
  | AtomReal Scientific
  | AtomString Text
  | AtomSymbol Text
  | AtomKeyword Text
    deriving (Show, Eq, Ord)

deriveStackPrisms ''Atom

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

fixP :: StackPrism (f (Fix f)) (Fix f)
fixP = stackPrism Fix (\(Fix f) -> Just f)

atomP :: StackPrism Atom (SexpF f)
atomP = stackPrism Atom $ \s ->
  case s of
    (Atom a) -> Just a
    _        -> Nothing

boolSexpP :: StackPrism (Bool :- t) (Sexp :- t)
boolSexpP = inStack (fixP . atomP) . _AtomBool

intSexpP :: StackPrism (Integer :- t) (Sexp :- t)
intSexpP = inStack (fixP . atomP) . _AtomInt

realSexpP :: StackPrism (Scientific :- t) (Sexp :- t)
realSexpP = inStack (fixP . atomP) . _AtomReal

stringSexpP :: StackPrism (Text :- t) (Sexp :- t)
stringSexpP = inStack (fixP . atomP) . _AtomString

symbolSexpP :: StackPrism (Text :- t) (Sexp :- t)
symbolSexpP = inStack (fixP . atomP) . _AtomSymbol

keywordSexpP :: StackPrism (Text :- t) (Sexp :- t)
keywordSexpP = inStack (fixP . atomP) . _AtomKeyword
