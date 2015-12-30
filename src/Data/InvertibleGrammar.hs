{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.InvertibleGrammar
  ( Grammar (..)
  , iso
  , embedPrism
  , push
  , pushForget
  , InvertibleGrammar(..)
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Control.Monad.Except
import Data.Semigroup
import Data.StackPrism

data Grammar g t t' where
  -- | Embed a prism which can fail during generation
  GenPrism :: String -> StackPrism a b -> Grammar g a b

  Iso :: (a -> b) -> (b -> a) -> Grammar g a b

  -- | Identity grammar
  Id :: Grammar g t t

  -- | Grammar composition
  (:.:) :: Grammar g b c -> Grammar g a b -> Grammar g a c

  -- | Grammar alternation
  (:<>:) :: Grammar g a b -> Grammar g a b -> Grammar g a b

  -- | Embed a subgrammar
  Inject :: g a b -> Grammar g a b

push :: (Eq a) => a -> Grammar g t (a :- t)
push a = GenPrism "push" $ stackPrism g f
  where
    g t = a :- t
    f (a' :- t) = if a == a' then Just t else Nothing


pushForget :: a -> Grammar g t (a :- t)
pushForget a = GenPrism "pushForget" $ stackPrism g f
  where
    g t = a :- t
    f (_ :- t) = Just t

iso :: (a -> b) -> (b -> a) -> Grammar g (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

embedPrism :: StackPrism a b -> Grammar g (a :- t) (b :- t)
embedPrism prism = GenPrism "custom prism" (stackPrism f g)
  where
    f (a :- t) = forward prism a :- t
    g (b :- t) = (:- t) <$> backward prism b

instance Category (Grammar c) where
  id = Id
  (.) Id y  = y
  (.) x  Id = x
  (.) x  y  = x :.: y

instance Semigroup (Grammar c t1 t2) where
  (<>) = (:<>:)

class InvertibleGrammar m g where
  parseWithGrammar :: g a b -> (a -> m b)
  genWithGrammar   :: g a b -> (b -> m a)

instance
  ( Monad m
  , MonadPlus m
  , MonadError String m
  , InvertibleGrammar m g
  ) => InvertibleGrammar m (Grammar g) where
  parseWithGrammar (Iso f _)      = return . f
  parseWithGrammar (GenPrism _ p) = return . forward p
  parseWithGrammar Id             = return
  parseWithGrammar (g :.: f)      = parseWithGrammar g <=< parseWithGrammar f
  parseWithGrammar (f :<>: g)     = \x -> parseWithGrammar f x `mplus` parseWithGrammar g x
  parseWithGrammar (Inject g)     = parseWithGrammar g

  genWithGrammar (Iso _ g)         = return . g
  genWithGrammar (GenPrism name p) = maybe (throwError $ "Cannot generate Sexp for: " ++ name) return . backward p
  genWithGrammar Id                = return
  genWithGrammar (g :.: f)         = genWithGrammar g >=> genWithGrammar f
  genWithGrammar (f :<>: g)        = \x -> genWithGrammar f x `mplus` genWithGrammar g x
  genWithGrammar (Inject g)        = genWithGrammar g

