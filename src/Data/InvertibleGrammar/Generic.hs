{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- NB: UndecidableInstances needed for nested type family application. :-/

module Data.InvertibleGrammar.Generic
  ( with
  , match
  , Coproduct (..)
  ) where

import Prelude hiding ((.), id)
import Control.Category ((.))
import Data.InvertibleGrammar
import Data.StackPrism
import Data.StackPrism.ReverseGeneric
import GHC.Generics

with
  :: forall a b s t g c d f.
     ( Generic a
     , MkPrismList (Rep a)
     , Rep a ~ M1 D d (M1 C c f)
     , StackPrismLhs f t ~ b
     , Constructor c
     ) =>
     Grammar g s b
  -> Grammar g s (a :- t)
with g =
  let PrismList (P prism) = mkRevPrismList
  in GenPrism (conName (undefined :: m c f e)) prism . g

match :: (Generic a, MkPrismList (Rep a), Match (Rep a) bs t2) => Coproduct t t1 bs t2 -> Grammar t (t1 :- t2) (a :- t2)
match lst = let (g, End) = match' mkRevPrismList lst in g

data Coproduct g s a t where
  With :: Grammar g (s :- t) b -> Coproduct g s bs t -> Coproduct g s (b ': bs) t
  End :: Coproduct g s '[] t

type family Trav (t :: * -> *) (l :: [*]) :: [*]
type instance Trav (M1 D c f) lst = Trav f lst
type instance Trav (f :+: g) lst = Trav g (Trav f lst)
type instance Trav (M1 C c f) (l ': ls) = ls

class Match (f :: * -> *) bs t where
  match' :: PrismList f a -> Coproduct g s bs t -> (Grammar g (s :- t) (a :- t), Coproduct g s (Trav f bs) t)

instance (Match f bs t) => Match (M1 D c f) bs t where
  match' (PrismList p) = match' p

instance
  ( Match f bs t
  , Match g (Trav f bs) t
  ) => Match (f :+: g) bs t where
  match' (p :& q) lst =
    let (gp, rest)  = match' p lst
        (qp, rest') = match' q rest
    in (gp :<>: qp, rest')

instance (StackPrismLhs f t ~ b, Constructor c) => Match (M1 C c f) (b ': bs) t where
  match' (P prism) (With g rest) =
    (GenPrism (conName (undefined :: m c f e)) prism . g, rest)
