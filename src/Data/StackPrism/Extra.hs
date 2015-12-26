{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Data.StackPrism.Extra
  ( inStack
  , iso
  , module Data.StackPrism
  ) where

import Data.StackPrism

inStack :: StackPrism a b -> StackPrism (a :- t) (b :- t)
inStack prism = stackPrism f g
  where
    f (a :- t) = forward prism a :- t
    g (b :- t) = (:- t) <$> backward prism b

iso :: (a -> b) -> (b -> a) -> StackPrism a b
iso fw bw = stackPrism fw (Just . bw)
