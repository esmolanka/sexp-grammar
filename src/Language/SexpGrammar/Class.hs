{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Language.SexpGrammar.Class where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category

import Data.InvertibleGrammar
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Language.Sexp.Located
import Language.SexpGrammar.Base
import Language.SexpGrammar.Generic

class SexpIso a where
  sexpIso :: Grammar Position (Sexp :- t) (a :- t)

instance SexpIso Bool where
  sexpIso =
    (sym "true"  >>> push True  (==True)  (const $ expected "Bool")) <>
    (sym "false" >>> push False (==False) (const $ expected "Bool"))

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
    pair

instance (Ord k, SexpIso k, SexpIso v) => SexpIso (Map k v) where
  sexpIso = iso Map.fromList Map.toList . braceList (rest sexpIso)

instance (Ord a, SexpIso a) => SexpIso (Set a) where
  sexpIso = iso Set.fromList Set.toList . braceList (rest sexpIso)

instance (SexpIso a) => SexpIso (Maybe a) where
  sexpIso = match
    $ With (\nothing -> sym "nil" >>> nothing)
    $ With (\just    -> list (el (sym "just") >>> el sexpIso) >>> just)
    $ End

instance (SexpIso a, SexpIso b) => SexpIso (Either a b) where
  sexpIso = match
    $ With (\left  -> list (el (sym "left")  >>> el sexpIso) >>> left)
    $ With (\right -> list (el (sym "right") >>> el sexpIso) >>> right)
    $ End

instance (SexpIso a) => SexpIso [a] where
  sexpIso = list $ rest sexpIso

instance (SexpIso a) => SexpIso (NE.NonEmpty a) where
  sexpIso =
    list (el sexpIso >>> rest sexpIso) >>>
    pair >>>
    iso (\(x,xs) -> x NE.:| xs )
        (\(x NE.:| xs) -> (x, xs))
