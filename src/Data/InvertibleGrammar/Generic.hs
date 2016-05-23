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
     , MkStackPrism f
     , Rep a ~ M1 D d (M1 C c f)
     , StackPrismLhs f t ~ b
     , Constructor c
     ) =>
     Grammar g s b
  -> Grammar g s (a :- t)
with g =
  let PrismList (P prism) = mkRevPrismList
  in GenPrism (conName (undefined :: m c f e)) prism . g

type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  (:++) (a ': as) bs = a ': (as :++ bs)
  (:++) '[] bs = bs

type family Coll (f :: * -> *) (t :: *) :: [*] where
  Coll (M1 D c f) t = Coll f t
  Coll (f :+: g)  t = Coll f t :++ Coll g t
  Coll (M1 C c f) t = '[StackPrismLhs f t]

match
  :: ( Generic a
     , MkPrismList (Rep a)
     , Match (Rep a) bs t
     , bs ~ Coll (Rep a) t
     ) =>
     Coproduct g s bs a t
  -> Grammar g s (a :- t)
match = fst . match' mkRevPrismList

data Coproduct g s bs a t where
  With
    :: (Grammar g b (a :- t) -> Grammar g s (a :- t))
    -> Coproduct g s bs a t
    -> Coproduct g s (b ': bs) a t

  End :: Coproduct g s '[] a t

type family Trav (t :: * -> *) (l :: [*]) :: [*] where
  Trav (M1 D c f) lst = Trav f lst
  Trav (f :+: g) lst = Trav g (Trav f lst)
  Trav (M1 C c f) (l ': ls) = ls

class Match (f :: * -> *) bs t where
  match' :: PrismList f a
         -> Coproduct g s bs a t
         -> ( Grammar g s (a :- t)
            , Coproduct g s (Trav f bs) a t
            )

instance (Match f bs t, Trav f bs ~ '[]) => Match (M1 D c f) bs t where
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
    (g $ GenPrism (conName (undefined :: m c f e)) prism, rest)
