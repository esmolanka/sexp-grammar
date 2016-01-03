{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Misc where

import Prelude hiding ((.), id)
import Control.Category
import Data.Text.Lazy (Text)
import Language.Sexp
import Language.SexpGrammar

newtype Ident = Ident String
  deriving (Show)

data Pair a b = Pair a b deriving (Show)

data Person = Person
  { pName :: String
  , pAddress :: String
  , pAge :: Maybe Int
  } deriving (Show)

return []

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso =
    list (                  -- begin list
      el sexpIso >>>        -- consume and push first element to stack:  (a :- t)
      el sexpIso            -- consume and push second element to stack: (b :- a :- t)
    ) >>>
    $(grammarFor 'Pair)     -- pop b, pop a, apply a to Pair,
                            -- apply b to (Pair a):                      (Pair a b :- t)

instance SexpIso Person where
  sexpIso = $(grammarFor 'Person) .
    list (
      el (sym "person") >>>
      el string' >>>
      props (
        Kw "address" .: string' >>>
        Kw "age" .:? int))

test :: String -> Grammar SexpGrammar (Sexp :- ()) (a :- ()) -> (a, Text)
test str g = either error id $ do
  sexp <- parseSexp "<input>" str
  expr <- parse g sexp
  sexp' <- gen g expr
  return (expr, printSexp sexp')
