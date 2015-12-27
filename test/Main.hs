{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Prelude hiding ((.), id)
import Control.Category

import Data.Semigroup
import Data.Functor.Foldable (Fix (..))
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Language.Sexp
import Language.SexpGrammar

pattern List' xs   = Fix (List xs)
pattern Bool' x    = Fix (Atom (AtomBool x))
pattern Int' x     = Fix (Atom (AtomInt x))
pattern Keyword' x = Fix (Atom (AtomKeyword x))
pattern Real' x    = Fix (Atom (AtomReal x))
pattern String' x  = Fix (Atom (AtomString x))
pattern Symbol' x  = Fix (Atom (AtomSymbol x))

data Pair a b = Pair a b
  deriving (Show, Eq, Ord, Generic)

data Foo a b = Bar a b
             | Baz a b
  deriving (Show, Eq, Ord, Generic)

data Rint = Rint Int

return []

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso = $(grammarFor 'Pair) . list (el sexpIso >>> el sexpIso)

instance (SexpIso a, SexpIso b) => SexpIso (Foo a b) where
  sexpIso = sconcat
    [ $(grammarFor 'Bar) . list (el (sym "bar") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Baz) . list (el (sym "baz") >>> el sexpIso >>> el sexpIso)
    ]

----------------------------------------------------------------------
-- Test cases

grammarTests :: TestTree
grammarTests = testGroup "Grammar tests"
  [ baseTypeTests
  , listTests
  , revStackPrismTests
  ]

baseTypeTests :: TestTree
baseTypeTests = testGroup "Base type combinator tests"
  [ testCase "bool" $
    parse bool (Bool' True) @?= Right True
  , testCase "integer" $
    parse integer (Int' (42 ^ (42 :: Integer))) @?= Right (42 ^ (42 :: Integer))
  , testCase "int" $
    parse int (Int' 65536) @?= Right 65536
  , testCase "real" $
    parse real (Real' 3.14) @?= Right 3.14
  , testCase "double" $
    parse double (Real' 3.14) @?= Right 3.14
  , testCase "string" $
    parse string (String' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "string'" $
    parse string' (String' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "keyword" $
    parse keyword (Keyword' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "keyword'" $
    parse keyword' (Keyword' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "symbol" $
    parse symbol (Symbol' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "symbol'" $
    parse symbol' (Symbol' "foo\nbar baz") @?= Right "foo\nbar baz"
  ]

listTests :: TestTree
listTests = testGroup "List combinator tests"
  [ testCase "empty list of bools" $
    parse (list (multiple (el bool))) (List' []) @?= Right []
  , testCase "list of bools" $
    Right [True, False, False] @=?
    parse (list (multiple (el bool))) (List' [Bool' True, Bool' False, Bool' False])
  ]

revStackPrismTests :: TestTree
revStackPrismTests = testGroup "Reverse stack prism tests"
  [ testCase "pair of two bools" $
    Right (Pair False True) @=?
    parse sexpIso (List' [Bool' False, Bool' True])
  , testCase "sum of products (Bar True 42)" $
    Right (Bar True (42 :: Int)) @=?
    parse sexpIso (List' [Symbol' "bar", Bool' True, Int' 42])
  , testCase "sum of products (Baz True False) tries to parse (baz #f 10)" $
    (Left "Expected bool, got something else" :: Either String (Foo Bool Bool)) @=?
    parse sexpIso (List' [Symbol' "baz", Bool' False, Int' 10])
  ]

main :: IO ()
main = defaultMain grammarTests
