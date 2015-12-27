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
  , InvertibleGrammar(..)
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Control.Monad.Except
import Data.Semigroup
import Data.StackPrism

data Grammar g t t' where
  -- | Embed a prism which can fail during parsing
  ParsePrism :: StackPrism b a -> Grammar g a b

  -- | Embed a prism which can fail during generation
  GenPrism :: StackPrism a b -> Grammar g a b

  -- | Identity grammar
  Id :: Grammar g t t

  -- | Grammar composition
  (:.:) :: Grammar g t' t'' -> Grammar g t t' -> Grammar g t t''

  -- | Grammar alternation
  (:<>:) :: Grammar g t t' -> Grammar g t t' -> Grammar g t t'

  -- | Grammar repeats
  Many :: Grammar g t t -> Grammar g t t

  -- | Embed a subgrammar
  Inject :: g a b -> Grammar g a b

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
  parseWithGrammar (ParsePrism prism) = \a ->
    case backward prism a of
      Nothing -> throwError "ParsePrism: unexpected input"
      Just a -> return a
  parseWithGrammar (GenPrism prism) = return . forward prism
  parseWithGrammar Id           = return
  parseWithGrammar (g :.: f)    = parseWithGrammar g <=< parseWithGrammar f
  parseWithGrammar (f :<>: g)   = \x -> parseWithGrammar f x `mplus` parseWithGrammar g x
  parseWithGrammar (Many g)     = go
    where
      go x = (parseWithGrammar g x >>= go) `mplus` return x
  parseWithGrammar (Inject g)     = parseWithGrammar g

  genWithGrammar (ParsePrism p) = return . forward p
  genWithGrammar (GenPrism p)   = maybe (error "cannot generate") return . backward p
  genWithGrammar Id             = return
  genWithGrammar (g :.: f)      = genWithGrammar g >=> genWithGrammar f
  genWithGrammar (f :<>: g)     = \x -> genWithGrammar f x `mplus` genWithGrammar g x
  genWithGrammar (Many g)       = go
    where
      go x = (genWithGrammar g x >>= go) `mplus` return x
  genWithGrammar (Inject g)     = genWithGrammar g

