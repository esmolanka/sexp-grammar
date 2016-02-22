{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE InstanceSigs           #-}

module Data.InvertibleGrammar.Generic
  ( match
  , Coproduct (..)
  ) where

import Prelude hiding ((.), id)

import Control.Category ((.))

import Data.InvertibleGrammar
import Data.StackPrism
import Data.StackPrism.ReverseGeneric
import GHC.Generics

match
  :: (Generic a, MkPrismList (Rep a), Match (Rep a) bs t) =>
     Coproduct g s bs t
  -> Grammar g (s :- t) (a :- t)
match =
  match' mkRevPrismList

data Coproduct g s a t where
  With :: Grammar g (s :- t) b -> Coproduct g s bs t -> Coproduct g s (b ': bs) t
  End :: Coproduct g s '[] t

class Match (f :: * -> *) bs t where
  match' :: PrismList f a -> Coproduct g s bs t -> Grammar g (s :- t) (a :- t)

instance (Match f bs t) => Match (M1 D c f) bs t where
  match' (PrismList p) = match' p

instance (Match f '[b] t, Match g bs t) => Match (f :+: g) (b ': bs) t where
  match' (p :& q) (With g cs) = match' p (With g End) :<>: match' q cs

instance (StackPrismLhs f t ~ b, Constructor c) => Match (M1 C c f) '[b] t where
  match' (P prism) (With g ~End) =
    GenPrism (conName (undefined :: m c f e)) prism . g
