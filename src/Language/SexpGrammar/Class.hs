{-# LANGUAGE TypeOperators #-}

module Language.SexpGrammar.Class where

import Prelude hiding ((.), id)

import Control.Category
import Data.Scientific
import Data.Text (Text)

import Language.SexpGrammar.Base
import Language.SexpGrammar.Combinators

class SexpIso a where
  sexpIso :: SexpG a

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

instance (SexpIso a, SexpIso b) => SexpIso (a, b) where
  sexpIso = pair . vect (el sexpIso >>> el sexpIso)

instance (SexpIso a) => SexpIso [a] where
  sexpIso = list $ multiple $ el sexpIso
