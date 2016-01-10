{-# LANGUAGE CPP                   #-}
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
  , embedParsePrism
  , push
  , pushForget
  , InvertibleGrammar(..)
  ) where

import Prelude hiding ((.), id)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Category
import Control.Monad
#if MIN_VERSION_mtl(2, 2, 0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Data.Semigroup
import Data.StackPrism

data Grammar g t t' where
  -- Embed a prism which can fail during generation
  GenPrism :: String -> StackPrism a b -> Grammar g a b

  -- Embed a prism which can fail during parsing
  ParsePrism :: String -> StackPrism b a -> Grammar g a b

  -- Embed an isomorphism that never fails
  Iso :: (a -> b) -> (b -> a) -> Grammar g a b

  -- Grammar composition
  (:.:) :: Grammar g b c -> Grammar g a b -> Grammar g a c

  -- Grammar alternation
  (:<>:) :: Grammar g a b -> Grammar g a b -> Grammar g a b

  -- Embed a subgrammar
  Inject :: g a b -> Grammar g a b

-- | Make a grammar from a total isomorphism on top element of stack
iso :: (a -> b) -> (b -> a) -> Grammar g (a :- t) (b :- t)
iso f' g' = Iso f g
  where
    f (a :- t) = f' a :- t
    g (b :- t) = g' b :- t

-- | Make a grammar from a prism which can fail during generation
embedPrism :: StackPrism a b -> Grammar g (a :- t) (b :- t)
embedPrism prism = GenPrism "custom prism" (stackPrism f g)
  where
    f (a :- t) = forward prism a :- t
    g (b :- t) = (:- t) <$> backward prism b

-- | Make a grammar from a prism which can fail during parsing
embedParsePrism :: String -> StackPrism b a -> Grammar g (a :- t) (b :- t)
embedParsePrism prismName prism = ParsePrism prismName (stackPrism f g)
  where
    f (a :- t) = forward prism a :- t
    g (b :- t) = (:- t) <$> backward prism b

-- | Unconditionally push given value on stack, i.e. it does not
-- consume anything on parsing. However such grammar expects the same
-- value as given one on stack during generation.
push :: (Eq a) => a -> Grammar g t (a :- t)
push a = GenPrism "push" $ stackPrism g f
  where
    g t = a :- t
    f (a' :- t) = if a == a' then Just t else Nothing

-- | Same as 'push' except it does not check the value on stack during
-- generation. Potentially unsafe as it \"forgets\" some data.
pushForget :: a -> Grammar g t (a :- t)
pushForget a = GenPrism "pushForget" $ stackPrism g f
  where
    g t = a :- t
    f (_ :- t) = Just t

instance Category (Grammar c) where
  id = Iso id id
  (.) x y = x :.: y

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
  parseWithGrammar (Iso f _)           = return . f
  parseWithGrammar (GenPrism _ p)      = return . forward p
  parseWithGrammar (ParsePrism name p) =
    maybe (throwError $ "Cannot parse Sexp for: " ++ name) return . backward p
  parseWithGrammar (g :.: f)           = parseWithGrammar g <=< parseWithGrammar f
  parseWithGrammar (f :<>: g)          =
    \x -> parseWithGrammar f x `mplus` parseWithGrammar g x
  parseWithGrammar (Inject g)          = parseWithGrammar g

  genWithGrammar (Iso _ g)         = return . g
  genWithGrammar (GenPrism name p) =
    maybe (throwError $ "Cannot generate Sexp for: " ++ name) return . backward p
  genWithGrammar (ParsePrism _ p)  = return . forward p
  genWithGrammar (g :.: f)         = genWithGrammar g >=> genWithGrammar f
  genWithGrammar (f :<>: g)        =
    \x -> genWithGrammar f x `mplus` genWithGrammar g x
  genWithGrammar (Inject g)        = genWithGrammar g

