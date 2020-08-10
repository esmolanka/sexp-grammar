{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE TypeOperators     #-}

module Language.SexpGrammar.Class
  ( SexpGrammar
  , SexpIso(..)
  ) where

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

-- | A common type of grammar that operates on S-expressions. This grammar
-- accepts a single 'Sexp' value and converts it into a value of type
-- @a@.
type SexpGrammar a = forall t. Grammar Position (Sexp :- t) (a :- t)

-- | A class for types that could be converted to and inferred from
-- s-expressions defined by 'Sexp'.
class SexpIso a where
  sexpIso :: SexpGrammar a

instance SexpIso () where
  sexpIso = with $ \unit ->
    sym "nil" >>> unit

instance SexpIso Bool where
  sexpIso = match
    $ With (\false_ -> sym "false" >>> false_)
    $ With (\true_ -> sym "true" >>> true_)
    $ End

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
    list (
      el sexpIso >>>
      el sexpIso) >>> pair

instance (SexpIso a, SexpIso b, SexpIso c) => SexpIso (a, b, c) where
  sexpIso = with $ \tuple3 ->
    list (
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso) >>> tuple3

instance (SexpIso a, SexpIso b, SexpIso c, SexpIso d) => SexpIso (a, b, c, d) where
  sexpIso = with $ \tuple4 ->
    list (
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso) >>> tuple4

instance (SexpIso a, SexpIso b, SexpIso c, SexpIso d, SexpIso e) => SexpIso (a, b, c, d, e) where
  sexpIso = with $ \tuple5 ->
    list (
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso) >>> tuple5

instance (SexpIso a, SexpIso b, SexpIso c, SexpIso d, SexpIso e, SexpIso f) => SexpIso (a, b, c, d, e, f) where
  sexpIso = with $ \tuple6 ->
    list (
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso) >>> tuple6

instance (SexpIso a, SexpIso b, SexpIso c, SexpIso d, SexpIso e, SexpIso f, SexpIso g) =>
         SexpIso (a, b, c, d, e, f, g) where
  sexpIso = with $ \tuple7 ->
    list (
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso >>>
      el sexpIso) >>> tuple7

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
