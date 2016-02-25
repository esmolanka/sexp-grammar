{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Misc where

import Prelude hiding ((.), id)
import Control.Category
import Data.Text.Lazy (Text)
import Data.InvertibleGrammar.Generic
import Language.Sexp
import Language.SexpGrammar

import GHC.Generics

newtype Ident = Ident String
  deriving (Show, Generic)

data Pair a b = Pair a b
  deriving (Show, Generic)

data Person = Person
  { pName :: String
  , pAddress :: String
  , pAge :: Maybe Int
  } deriving (Show, Generic)

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso =
    -- Combinator 'with' matches the single constructor of a datatype to a grammar
    with $                  -- pops b, pops a, applies a to Pair,
                            -- apply b to (Pair a):                      (Pair a b :- t)
    list (                  -- begin list
      el sexpIso >>>        -- consume and push first element to stack:  (a :- t)
      el sexpIso            -- consume and push second element to stack: (b :- a :- t)
    )

instance SexpIso Person where
  sexpIso = with $
    list (
      el (sym "person") >>>
      el string'        >>>
      props (
        Kw "address" .:  string' >>>
        Kw "age"     .:? int))

data FooBar
  = Foo Int Double
  | Bar Bool
    deriving (Show, Generic)

foobarSexp :: SexpG FooBar
foobarSexp =
  match $
    With (list (el int >>> el double)) $
    With bool $
    End

test :: String -> SexpG a -> (a, Text)
test str g = either error id $ do
  e <- parseFromString g str
  sexp' <- gen g e
  return (e, printSexp sexp')
