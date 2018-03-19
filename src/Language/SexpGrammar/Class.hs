{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Language.SexpGrammar.Class where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category

import Data.InvertibleGrammar
import Data.InvertibleGrammar.TH
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Sexp.Types
import Language.SexpGrammar.Base

class SexpIso a where
  sexpIso :: Grammar Position (Sexp :- t) (a :- t)

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
  sexpIso =
    list (el sexpIso >>> el (sym ".") >>> el sexpIso) >>>
    swap >>>
    Flip pair

instance (Ord k, SexpIso k, SexpIso v) => SexpIso (Map k v) where
  sexpIso = iso Map.fromList Map.toList . list (el sexpIso)

instance (Ord a, SexpIso a) => SexpIso (Set a) where
  sexpIso = iso Set.fromList Set.toList . list (el sexpIso)

instance (SexpIso a) => SexpIso (Maybe a) where
  sexpIso = coproduct
    [ $(grammarFor 'Nothing) . kw (Kw "nil")
    , $(grammarFor 'Just) . sexpIso
    ]

instance (SexpIso a) => SexpIso [a] where
  sexpIso = list $ rest sexpIso

instance (SexpIso a) => SexpIso (NE.NonEmpty a) where
  sexpIso =
    list (el sexpIso >>> rest sexpIso) >>>
    swap >>>
    Flip pair >>>
    iso (\(x,xs) -> x NE.:| xs )
        (\(x NE.:| xs) -> (x, xs))
