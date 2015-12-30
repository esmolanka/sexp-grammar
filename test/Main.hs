{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import Prelude hiding ((.), id)

import Control.Category
import Data.Semigroup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck ()

import Language.Sexp
import Language.SexpGrammar

pattern List' xs   = List (Position 0 0) xs
pattern Bool' x    = Atom (Position 0 0) (AtomBool x)
pattern Int' x     = Atom (Position 0 0) (AtomInt x)
pattern Keyword' x = Atom (Position 0 0) (AtomKeyword x)
pattern Real' x    = Atom (Position 0 0) (AtomReal x)
pattern String' x  = Atom (Position 0 0) (AtomString x)
pattern Symbol' x  = Atom (Position 0 0) (AtomSymbol x)

data Pair a b = Pair a b
  deriving (Show, Eq, Ord)

data Foo a b = Bar a b
             | Baz a b
  deriving (Show, Eq, Ord)

data Rint = Rint Int

data ArithExpr =
    Lit Int
  | Add ArithExpr ArithExpr -- ^ (+ x y)
  | Mul [ArithExpr] -- ^ (* x1 ... xN)
  deriving (Show, Eq, Ord)

instance Arbitrary ArithExpr where
  arbitrary = frequency
    [ (5, Lit <$> arbitrary)
    , (1, Add <$> arbitrary <*> arbitrary)
    , (1, do
          n <- choose (0, 7)
          Mul <$> vectorOf n arbitrary)
    ]

arithExprParseGenProp :: ArithExpr -> Bool
arithExprParseGenProp expr =
  (gen arithExprGrammar expr >>= parse arithExprGrammar :: Either String ArithExpr)
  ==
  Right expr
  where
    arithExprGrammar :: Grammar SexpGrammar (Sexp :- t) (ArithExpr :- t)
    arithExprGrammar = sexpIso

return []

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso = $(grammarFor 'Pair) . list (el sexpIso >>> el sexpIso)

instance (SexpIso a, SexpIso b) => SexpIso (Foo a b) where
  sexpIso = sconcat
    [ $(grammarFor 'Bar) . list (el (sym "bar") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Baz) . list (el (sym "baz") >>> el sexpIso >>> el sexpIso)
    ]

instance SexpIso ArithExpr where
  sexpIso = sconcat
    [ $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> rest sexpIso)
    ]

----------------------------------------------------------------------
-- Test cases

grammarTests :: TestTree
grammarTests = testGroup "Grammar tests"
  [ baseTypeTests
  , listTests
  , revStackPrismTests
  , parseTests
  , genTests
  , parseGenTests
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
    parse keyword (Keyword' (Kw "foobarbaz")) @?= Right (Kw "foobarbaz")
  , testCase "symbol" $
    parse symbol (Symbol' "foobarbaz") @?= Right "foobarbaz"
  , testCase "symbol'" $
    parse symbol' (Symbol' "foobarbaz") @?= Right "foobarbaz"
  ]

listTests :: TestTree
listTests = testGroup "List combinator tests"
  [ testCase "empty list of bools" $
    parse (list (rest bool)) (List' []) @?= Right []
  , testCase "list of bools" $
    Right [True, False, False] @=?
    parse (list (rest bool)) (List' [Bool' True, Bool' False, Bool' False])
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


testArithExpr :: ArithExpr
testArithExpr = Add (Lit 0) (Mul [])

testArithExprSexp :: Sexp
testArithExprSexp = List' [Symbol' "+", Int' 0, List' [Symbol' "*"]]

parseTests :: TestTree
parseTests = testGroup "parse tests"
  [ testCase "(+ 0 (*))" $
    Right testArithExpr @=? parse sexpIso testArithExprSexp
  ]

genTests :: TestTree
genTests = testGroup "gen tests"
  [ testCase "(+ 0 (*))" $
    Right testArithExprSexp @=? gen sexpIso testArithExpr
  ]

parseGenTests :: TestTree
parseGenTests = testGroup "parse . gen == id"
  [ QC.testProperty "ArithExprs" arithExprParseGenProp
  ]

main :: IO ()
main = defaultMain grammarTests
