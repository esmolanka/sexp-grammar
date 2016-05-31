{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import Prelude hiding ((.), id)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Category
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Scientific
import Data.Semigroup
import Test.QuickCheck ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import GHC.Generics

import Language.Sexp as Sexp
import Language.SexpGrammar as G
import Language.SexpGrammar.Generic
import Language.SexpGrammar.TH hiding (match)

pattern List' xs   = List (Position 0 0) xs
pattern Bool' x    = Atom (Position 0 0) (AtomBool x)
pattern Int' x     = Atom (Position 0 0) (AtomInt x)
pattern Keyword' x = Atom (Position 0 0) (AtomKeyword x)
pattern Real' x    = Atom (Position 0 0) (AtomReal x)
pattern String' x  = Atom (Position 0 0) (AtomString x)
pattern Symbol' x  = Atom (Position 0 0) (AtomSymbol x)

stripPos :: Sexp -> Sexp
stripPos (Atom _ x)    = Atom dummyPos x
stripPos (List _ xs)   = List dummyPos $ map stripPos xs
stripPos (Vector _ xs) = Vector dummyPos $ map stripPos xs
stripPos (Quoted _ x)  = Quoted dummyPos $ stripPos x

parseSexp' :: String -> Either String Sexp
parseSexp' input = stripPos <$> Sexp.decode (B8.pack input)

data Pair a b = Pair a b
  deriving (Show, Eq, Ord, Generic)

data Foo a b = Bar a b
             | Baz a b
  deriving (Show, Eq, Ord, Generic)

data Rint = Rint Int

data ArithExpr =
    Lit Int
  | Add ArithExpr ArithExpr -- ^ (+ x y)
  | Mul [ArithExpr] -- ^ (* x1 ... xN)
  deriving (Show, Eq, Ord, Generic)

return []

instance Arbitrary ArithExpr where
  arbitrary = frequency
    [ (5, Lit <$> arbitrary)
    , (1, Add <$> arbitrary <*> arbitrary)
    , (1, do
          n <- choose (0, 7)
          Mul <$> vectorOf n arbitrary)
    ]

arithExprTHProp :: ArithExpr -> Bool
arithExprTHProp expr =
  (G.genSexp arithExprGrammar expr >>= G.parseSexp arithExprGrammar :: Either String ArithExpr)
  ==
  Right expr
  where
    arithExprGrammar :: Grammar SexpGrammar (Sexp :- t) (ArithExpr :- t)
    arithExprGrammar = sexpIso

arithExprGenericsProp :: ArithExpr -> Bool
arithExprGenericsProp expr =
  (G.genSexp arithExprGenericIso expr >>= G.parseSexp arithExprGenericIso :: Either String ArithExpr)
  ==
  Right expr

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso = $(grammarFor 'Pair) . list (el sexpIso >>> el sexpIso)

pairGenericIso :: SexpG a -> SexpG b -> SexpG (Pair a b)
pairGenericIso a b = with (list (el a >>> el b))

instance (SexpIso a, SexpIso b) => SexpIso (Foo a b) where
  sexpIso = sconcat
    [ $(grammarFor 'Bar) . list (el (sym "bar") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Baz) . list (el (sym "baz") >>> el sexpIso >>> el sexpIso)
    ]

fooGenericIso :: SexpG a -> SexpG b -> SexpG (Foo a b)
fooGenericIso a b = match
  $ With (\bar -> bar . list (el (sym "bar") >>> el a >>> el b))
  $ With (\baz -> baz . list (el (sym "baz") >>> el a >>> el b))
  $ End

instance SexpIso ArithExpr where
  sexpIso = sconcat
    [ $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> rest sexpIso)
    ]

arithExprGenericIso :: SexpG ArithExpr
arithExprGenericIso = expr
  where
    expr :: SexpG ArithExpr
    expr = match
      $ With (\lit -> lit . int)
      $ With (\add -> add . list (el (sym "+") >>> el expr >>> el expr))
      $ With (\mul -> mul . list (el (sym "*") >>> rest expr))
      $ End

----------------------------------------------------------------------
-- Test cases

allTests :: TestTree
allTests = testGroup "All tests"
  [ lexerTests
  , grammarTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "Lexer tests"
  [ testCase "123 is an integer number" $
    parseSexp' "123" @?= Right (Int' 123)
  , testCase "+123 is an integer number" $
    parseSexp' "+123" @?= Right (Int' 123)
  , testCase "-123 is an integer number" $
    parseSexp' "-123" @?= Right (Int' (- 123))
  , testCase "+123.4e5 is a floating number" $
    parseSexp' "+123.4e5" @?= Right (Real' (read "+123.4e5" :: Scientific))
  ]

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
    G.parseSexp bool (Bool' True) @?= Right True
  , testCase "integer" $
    G.parseSexp integer (Int' (42 ^ (42 :: Integer))) @?= Right (42 ^ (42 :: Integer))
  , testCase "int" $
    G.parseSexp int (Int' 65536) @?= Right 65536
  , testCase "real" $
    G.parseSexp real (Real' 3.14) @?= Right 3.14
  , testCase "double" $
    G.parseSexp double (Real' 3.14) @?= Right 3.14
  , testCase "string" $
    G.parseSexp string (String' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "string'" $
    G.parseSexp string' (String' "foo\nbar baz") @?= Right "foo\nbar baz"
  , testCase "keyword" $
    G.parseSexp keyword (Keyword' (Kw "foobarbaz")) @?= Right (Kw "foobarbaz")
  , testCase "symbol" $
    G.parseSexp symbol (Symbol' "foobarbaz") @?= Right "foobarbaz"
  , testCase "symbol'" $
    G.parseSexp symbol' (Symbol' "foobarbaz") @?= Right "foobarbaz"
  ]

listTests :: TestTree
listTests = testGroup "List combinator tests"
  [ testCase "empty list of bools" $
    G.parseSexp (list (rest bool)) (List' []) @?= Right []
  , testCase "list of bools" $
    G.parseSexp (list (rest bool)) (List' [Bool' True, Bool' False, Bool' False]) @?=
    Right [True, False, False]
  ]

revStackPrismTests :: TestTree
revStackPrismTests = testGroup "Reverse stack prism tests"
  [ testCase "pair of two bools" $
    G.parseSexp sexpIso (List' [Bool' False, Bool' True]) @?=
    Right (Pair False True)
  , testCase "sum of products (Bar True 42)" $
    G.parseSexp sexpIso (List' [Symbol' "bar", Bool' True, Int' 42]) @?=
    Right (Bar True (42 :: Int))
  , testCase "sum of products (Baz True False) tries to parse (baz #f 10)" $
    G.parseSexp sexpIso (List' [Symbol' "baz", Bool' False, Int' 10]) @?=
    (Left ("0:0: mismatch:\nexpected: atom of type bool\n     got: 10\n") :: Either String (Foo Bool Bool))
  ]

testArithExpr :: ArithExpr
testArithExpr = Add (Lit 0) (Mul [])

testArithExprSexp :: Sexp
testArithExprSexp = List' [Symbol' "+", Int' 0, List' [Symbol' "*"]]

parseTests :: TestTree
parseTests = testGroup "parse tests"
  [ testCase "(+ 0 (*))" $
    Right testArithExpr @=? G.parseSexp sexpIso testArithExprSexp
  ]

genTests :: TestTree
genTests = testGroup "gen tests"
  [ testCase "(+ 0 (*))" $
    Right testArithExprSexp @=? G.genSexp sexpIso testArithExpr
  ]

parseGenTests :: TestTree
parseGenTests = testGroup "parse . gen == id"
  [ QC.testProperty "ArithExprs TH" arithExprTHProp
  , QC.testProperty "ArithExprs Generics" arithExprGenericsProp
  ]

main :: IO ()
main = defaultMain allTests
