{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Misc where

import Prelude hiding ((.), id)

import Control.Category
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Text (Text)
import qualified Data.Text.Lazy as T


import qualified Language.Sexp as Sexp
import Language.SexpGrammar
import Language.SexpGrammar.Generic

import GHC.Generics

newtype Ident = Ident String
  deriving (Show, Generic)

data Pair a b = Pair a b
  deriving (Show, Generic)

data Person = Person
  { pName :: Text
  , pAddress :: Text
  , pAge :: Maybe Int
  } deriving (Show, Generic)

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso =
    -- Combinator 'with' matches the single constructor of a datatype to a grammar
    with $ \_Pair ->        -- pops b, pops a, applies a to Pair,
                            -- apply b to (Pair a):                      (Pair a b :- t)
    list (                  -- begin list
      el sexpIso >>>        -- consume and push first element to stack:  (a :- t)
      el sexpIso            -- consume and push second element to stack: (b :- a :- t)
    ) >>> _Pair

instance SexpIso Person where
  sexpIso = with $ \person ->
    list (
      el (sym "person") >>>
      el string         >>>
      props (
        "address" .:  string >>>
        "age"     .:? int))  >>>
    person


data FooBar a
  = Foo Int Double
  | Bar a
    deriving (Show, Generic)

foobarSexp :: SexpGrammar (FooBar Int)
foobarSexp =
  match $
    With (\foo -> foo . list (el int >>> el double)) $
    With (\bar -> bar . int) $
    End

test :: String -> SexpGrammar a -> (a, String)
test str g = either error id $ do
  e <- decodeWith g (B8.pack str)
  sexp' <- toSexp g e
  return (e, T.unpack (Sexp.prettySexp sexp'))
