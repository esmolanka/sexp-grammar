{-# LANGUAGE TypeOperators #-}

module Language.SexpGrammar.Class where

import Data.Functor.Foldable (Fix (..))
import Data.StackPrism

import Data.InvertibleGrammar
import Language.Sexp.Types

import Language.SexpGrammar.Base

class FromSexp a where
  -- | Convert Sexp into a
  sexpGrammar :: Grammar SexpGrammar (Sexp :- t) (a :- t)

instance FromSexp Bool where
  sexpGrammar = fromRevStackPrism $ stackPrismFirst p
    where
      p :: StackPrism Bool Sexp
      p = stackPrism (Fix . Atom . AtomBool) g
      g (Fix (Atom (AtomBool b))) = Just b
      g _                         = Nothing

instance (FromSexp a) => FromSexp [a] where
  sexpGrammar =
    Inject $ GList $
    multiple $ Inject $ GElem "sexpGrammar for [a]" sexpGrammar
