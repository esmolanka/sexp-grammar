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
{-# LANGUAGE RankNTypes             #-}
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

import Control.Applicative
import Control.Category ((.))

import Data.Functor.Identity
import Data.InvertibleGrammar
import Data.Monoid (First(..))
import Data.Profunctor (Choice(..))
import Data.Profunctor.Unsafe
import Data.Tagged
import Data.Text (pack)

import GHC.Generics

-- | Provide a data constructor/stack isomorphism to a grammar working on
-- stacks. Works for types with one data constructor. For sum types use 'match'
-- and 'Coproduct'.
with
  :: forall a b s t g c d f.
     ( Generic a
     , MkPrismList (Rep a)
     , MkStackPrism f
     , Rep a ~ M1 D d (M1 C c f)
     , StackPrismLhs f t ~ b
     , Constructor c
     ) =>
     (Grammar g b (a :- t) -> Grammar g s (a :- t))
  -> Grammar g s (a :- t)
with g =
  let PrismList (P prism) = mkRevPrismList
      name = conName (undefined :: m c f e)
  in g (PartialIso
         name
         (fwd prism)
         (maybe (Left $ expected (pack name)) Right . bkwd prism))

-- | Combine all grammars provided in 'Coproduct' list into a single grammar.
match
  :: ( Generic a
     , MkPrismList (Rep a)
     , Match (Rep a) bs t
     , bs ~ Coll (Rep a) t
     ) =>
     Coproduct g s bs a t
  -> Grammar g s (a :- t)
match = fst . match' mkRevPrismList

-- | Heterogenous list of grammars, each one matches a data constructor of type
-- @a@. 'With' is used to provide a data constructor/stack isomorphism to a
-- grammar working on stacks. 'End' ends the list of matches.
data Coproduct g s bs a t where

  With
    :: (Grammar g b (a :- t) -> Grammar g s (a :- t))
    -> Coproduct g s bs a t
    -> Coproduct g s (b ': bs) a t

  End :: Coproduct g s '[] a t

----------------------------------------------------------------------
-- Machinery

type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  (:++) (a ': as) bs = a ': (as :++ bs)
  (:++) '[] bs = bs

type family Coll (f :: * -> *) (t :: *) :: [*] where
  Coll (M1 D c f) t = Coll f t
  Coll (f :+: g)  t = Coll f t :++ Coll g t
  Coll (M1 C c f) t = '[StackPrismLhs f t]

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
    let name = conName (undefined :: m c f e)
        p = fwd prism
        q = maybe (Left $ expected (pack name)) Right . bkwd prism
    in (g $ PartialIso name p q, rest)

-- NB. The following machinery is heavily based on
-- https://github.com/MedeaMelana/stack-prism/blob/master/Data/StackPrism/Generic.hs

-- | Derive a list of stack prisms. For more information on the shape of a
-- 'PrismList', please see the documentation below.
mkRevPrismList :: (Generic a, MkPrismList (Rep a)) => StackPrisms a
mkRevPrismList = mkPrismList' to (Just . from)

type StackPrism a b = forall p f. (Choice p, Applicative f) => p a (f a) -> p b (f b)

-- | Construct a prism.
stackPrism :: (a -> b) -> (b -> Maybe a) -> StackPrism a b
stackPrism f g = dimap (\b -> maybe (Left b) Right (g b)) (either pure (fmap f)) . right'

-- | Apply a prism in forward direction.
fwd :: StackPrism a b -> a -> b
fwd l = runIdentity #. unTagged #. l .# Tagged .# Identity

-- | Apply a prism in backward direction.
bkwd :: StackPrism a b -> b -> Maybe a
bkwd l = getFirst #. getConst #. l (Const #. First #. Just)

-- | Convenient shorthand for a 'PrismList' indexed by a type and its generic
-- representation.
type StackPrisms a = PrismList (Rep a) a

-- | A data family that is indexed on the building blocks from representation
-- types from @GHC.Generics@. It builds up to a list of prisms, one for each
-- constructor in the generic representation. The list is wrapped in the unary
-- constructor @PrismList@. Within that constructor, the prisms are separated by
-- the right-associative binary infix constructor @:&@. Finally, the individual
-- prisms are wrapped in the unary constructor @P@.
--
-- As an example, here is how to define the prisms @nil@ and @cons@ for @[a]@,
-- which is an instance of @Generic@:
--
-- > nil  :: StackPrism              t  ([a] :- t)
-- > cons :: StackPrism (a :- [a] :- t) ([a] :- t)
-- > PrismList (P nil :& P cons) = mkPrismList :: StackPrisms [a]
data family PrismList (f :: * -> *) (a :: *)

class MkPrismList (f :: * -> *) where
  mkPrismList' :: (f p -> a) -> (a -> Maybe (f q)) -> PrismList f a

data instance PrismList (M1 D c f) a = PrismList (PrismList f a)

instance MkPrismList f => MkPrismList (M1 D c f) where
  mkPrismList' f' g' = PrismList (mkPrismList' (f' . M1) (fmap unM1 . g'))

infixr :&
data instance PrismList (f :+: g) a = PrismList f a :& PrismList g a

instance (MkPrismList f, MkPrismList g) => MkPrismList (f :+: g) where
  mkPrismList' f' g' = f f' g' :& g f' g'
    where
      f :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> PrismList f a
      f _f' _g' = mkPrismList' (\fp -> _f' (L1 fp)) (matchL _g')
      g :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> PrismList g a
      g _f' _g' = mkPrismList' (\gp -> _f' (R1 gp)) (matchR _g')

      matchL :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (f q)
      matchL _g' a = case _g' a of
        Just (L1 f'') -> Just f''
        _ -> Nothing

      matchR :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (g q)
      matchR _g' a = case _g' a of
        Just (R1 g'') -> Just g''
        _ -> Nothing

data instance PrismList (M1 C c f) a = P (forall t. StackPrism (StackPrismLhs f t) (a :- t))

instance MkStackPrism f => MkPrismList (M1 C c f) where
  mkPrismList' f' g' = P (stackPrism (f f') (g g'))
    where
      f :: forall a p t. (M1 C c f p -> a) -> StackPrismLhs f t -> a :- t
      f _f' lhs = mapHead (_f' . M1) (mkR lhs)
      g :: forall a p t. (a -> Maybe (M1 C c f p)) -> (a :- t) -> Maybe (StackPrismLhs f t)
      g _g' (a :- t) = fmap (mkL . (:- t) . unM1) (_g' a)

-- Deriving types and conversions for single constructors

type family StackPrismLhs (f :: * -> *) (t :: *) :: *

class MkStackPrism (f :: * -> *) where
  mkR :: forall p t. StackPrismLhs f t -> (f p :- t)
  mkL :: forall p t. (f p :- t) -> StackPrismLhs f t

type instance StackPrismLhs U1 t = t
instance MkStackPrism U1 where
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

type instance StackPrismLhs (K1 i a) t = a :- t
instance MkStackPrism (K1 i a) where
  mkR (h :- t) = K1 h :- t
  mkL (K1 h :- t) = h :- t

type instance StackPrismLhs (M1 i c f) t = StackPrismLhs f t
instance MkStackPrism f => MkStackPrism (M1 i c f) where
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

type instance StackPrismLhs (f :*: g) t = StackPrismLhs g (StackPrismLhs f t)
instance (MkStackPrism f, MkStackPrism g) => MkStackPrism (f :*: g) where
  mkR t = (hg :*: hf) :- tg
    where
      hf :- tf = mkR t
      hg :- tg = mkR tf
  mkL ((hf :*: hg) :- t) = mkL (hg :- mkL (hf :- t))

mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
