{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Language.SexpGrammar.Class where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category

import Data.InvertibleGrammar.TH
import qualified Data.List.NonEmpty as NE
import Data.Data
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Sexp
import Language.Sexp.Utils
import Language.SexpGrammar.Base
import Language.SexpGrammar.Combinators

getEnumName :: (Data a) => a -> Text
getEnumName = Text.pack . lispifyName . showConstr . toConstr

class SexpIso a where
  sexpIso :: SexpG a
  default sexpIso :: (Enum a, Bounded a, Eq a, Data a) => SexpG a
  sexpIso = coproduct $ map (\a -> push a . sym (getEnumName a)) [minBound .. maxBound]

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
    iso (\(x,xs) -> x NE.:| xs )
        (\(x NE.:| xs) -> (x, xs)) .
    pair .
    list (el sexpIso >>> rest sexpIso)
