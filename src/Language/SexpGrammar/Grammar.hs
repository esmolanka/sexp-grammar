{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Language.SexpGrammar.Grammar
  ( Grammar (..)
  , Context (..)
  ) where

import Prelude hiding ((.), id)
import Control.Category

data Context
  = InSexp
  | InList
  | InVector
  | InPair
  | InAtom

infixr 5 :-
data a :- b = a :- b
  deriving (Functor)

data Grammar (c :: Context) t1 t2 where
  Id    :: Grammar c t t
  (:.)  :: Grammar c t2 t3 -> Grammar c t1 t2 -> Grammar c t1 t3

  Empty :: Grammar c t1 t2
  (:<>) :: Grammar c t1 t2 -> Grammar c t1 t2 -> Grammar c t1 t2

instance Category (Grammar c) where
  id = Id
  (.) = (:.)

instance Monoid (Grammar c t1 t2) where
  mempty = Empty
  mappend = (:<>)
