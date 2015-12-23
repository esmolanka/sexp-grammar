{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import Prelude hiding ((.), id)

import Control.Category
import Data.Functor.Foldable (Fix (..))
import Data.StackPrism
import Data.StackPrism.Generic
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC

import Language.Sexp
import Language.SexpGrammar

pattern Nil'      = Fix Nil
pattern List' xs  = Fix (List xs Nil')
pattern Symbol' x = Fix (Atom (AtomSymbol x))
pattern Bool' x   = Fix (Atom (AtomBool x))

allTests :: TestTree
allTests = testGroup "all tests"
  [ grammarTests
  ]

data Pair a b = Pair a b
  deriving (Show, Eq, Ord, Generic)

instance (FromSexp a, FromSexp b) => FromSexp (Pair a b) where
  sexpGrammar = fromStackPrism pairPrism . list (fromSexp >>> fromSexp)
    where
      pairPrism :: StackPrism (a :- b :- t) (Pair a b :- t)
      PrismList (P pairPrism) = mkPrismList :: StackPrisms (Pair a' b')

grammarTests :: TestTree
grammarTests = testGroup "grammar tests"
  [ testCase "empty list of bools" $
    parse (list (multiple bool)) Nil' @?= Right []
  , testCase "list of bools" $
    Right [True, False, False] @=?
    parse (list (multiple bool)) (List' [Bool' True, Bool' False, Bool' False])
  , testCase "pair of two bools" $
    Right (Pair False True) @=?
    parse sexpGrammar (List' [Bool' False, Bool' True])
  ]

main :: IO ()
main = defaultMain allTests
