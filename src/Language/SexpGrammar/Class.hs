{-# LANGUAGE TypeOperators #-}

module Language.SexpGrammar.Class where

import Data.StackPrism.Extra
import Data.InvertibleGrammar
import Data.Scientific
import Data.Text (Text)
import Language.Sexp.Types

import Language.SexpGrammar.Base
import Language.SexpGrammar.Combinators

class SexpIso a where
  sexpIso :: Grammar SexpGrammar (Sexp :- t) (a :- t)

instance SexpIso Bool where
  sexpIso = bool

instance SexpIso Int where
  sexpIso = int

instance SexpIso Integer where
  sexpIso = integer

instance SexpIso Double where
  sexpIso = double

instance SexpIso Scientific where
  sexpIso = real

instance SexpIso Text where
  sexpIso = string

instance (SexpIso a) => SexpIso [a] where
  sexpIso = list $ multiple $ el sexpIso

