{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main (main) where

import Prelude hiding ((.), id)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Category
import qualified Data.Map as M
import Data.Scientific
import Data.Semigroup
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Test.QuickCheck ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Language.Sexp as Sexp hiding (parseSexp')
import Language.SexpGrammar as G
import Language.SexpGrammar.Generic
import Language.SexpGrammar.TH hiding (match)

pattern List' xs   = List (Position "<no location information>" 1 0) xs
pattern Bool' x    = Atom (Position "<no location information>" 1 0) (AtomBool x)
pattern Int' x     = Atom (Position "<no location information>" 1 0) (AtomInt x)
pattern Keyword' x = Atom (Position "<no location information>" 1 0) (AtomKeyword x)
pattern Real' x    = Atom (Position "<no location information>" 1 0) (AtomReal x)
pattern String' x  = Atom (Position "<no location information>" 1 0) (AtomString x)
pattern Symbol' x  = Atom (Position "<no location information>" 1 0) (AtomSymbol x)

stripPos :: Sexp -> Sexp
stripPos (Atom _ x)    = Atom dummyPos x
stripPos (List _ xs)   = List dummyPos $ map stripPos xs
stripPos (Vector _ xs) = Vector dummyPos $ map stripPos xs
stripPos (Quoted _ x)  = Quoted dummyPos $ stripPos x

parseSexp' :: String -> Either String Sexp
parseSexp' input = stripPos <$> Sexp.decode (TL.pack input)

data Pair a b = Pair a b
  deriving (Show, Eq, Ord, Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

data Foo a b = Bar a b
             | Baz a b
  deriving (Show, Eq, Ord, Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Foo a b) where
  arbitrary =
    frequency
      [ (1, Bar <$> arbitrary <*> arbitrary)
      , (1, Baz <$> arbitrary <*> arbitrary)
      ]

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

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso = $(grammarFor 'Pair) . list (el sexpIso >>> el sexpIso)

pairGenericIso
  :: (forall t. Grammar Position (Sexp :- t) (a :- t))
  -> (forall t. Grammar Position (Sexp :- t) (b :- t)) -> Grammar Position (Sexp :- t) (Pair a b :- t)
pairGenericIso a b = with (\pair -> pair . list (el a >>> el b))

instance (SexpIso a, SexpIso b) => SexpIso (Foo a b) where
  sexpIso = sconcat
    [ $(grammarFor 'Bar) . list (el (sym "bar") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Baz) . list (el (sym "baz") >>> el sexpIso >>> el sexpIso)
    ]

fooGenericIso
  :: (forall t. Grammar Position (Sexp :- t) (a :- t))
  -> (forall t. Grammar Position (Sexp :- t) (b :- t)) -> Grammar Position (Sexp :- t) (Foo a b :- t)
fooGenericIso a b = match
  $ With (\bar -> bar . list (el (sym "bar") >>> el a >>> el b))
  $ With (\baz -> baz . list (el (sym "baz") >>> el a >>> el b))
  $ End


arithExprTHIso :: Grammar Position (Sexp :- t) (ArithExpr :- t)
arithExprTHIso =
  sconcat
    [ $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el arithExprTHIso >>> el arithExprTHIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> rest arithExprTHIso)
    ]

arithExprGenericIso :: Grammar Position (Sexp :- t) (ArithExpr :- t)
arithExprGenericIso = expr
  where
    expr :: Grammar Position (Sexp :- t) (ArithExpr :- t)
    expr = match
      $ With (\lit -> lit . int)
      $ With (\add -> add . list (el (sym "+") >>> el expr >>> el expr))
      $ With (\mul -> mul . list (el (sym "*") >>> rest expr))
      $ End

data Person = Person
  { pName     :: String
  , pAge      :: Int
  , pAddress  :: String
  , pChildren :: [Person]
  } deriving (Show, Eq, Generic)


instance Arbitrary Person where
  arbitrary =
    Person
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> frequency
            [ (6, pure [])
            , (4, vectorOf 1 arbitrary)
            , (2, vectorOf 2 arbitrary)
            , (1, vectorOf 3 arbitrary)
            ]

personGenericIso :: Grammar Position (Sexp :- t) (Person :- t)
personGenericIso = with
  (\person ->
     list (
      el (sym "person") >>>
      el string' >>>
      props (
       Kw "age" .: int >>>
       Kw "address" .: string') >>>
      rest personGenericIso) >>> person)

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
  , testCase "comments" $
    parseSexp' ";; hello, world\n   123" @?= Right (Int' 123)
  , testCase "cyrillic characters in comments" $
    parseSexp' ";; привет!\n   123" @?= Right (Int' 123)
  , testCase "unicode math in comments" $
    parseSexp' ";; Γ ctx\n;; ----- Nat-formation\n;; Γ ⊦ Nat : Type\nfoobar" @?=
      Right (Symbol' "foobar")
  , testCase "symbol" $
    parseSexp' "hello-world" @?= Right (Symbol' "hello-world")
  , testCase "cyrillic symbol" $
    parseSexp' "привет-мир" @?= Right (Symbol' "привет-мир")
  , testCase "string with arabic characters" $
    parseSexp' "\"ي الخاطفة الجديدة، مع, بلديهم\"" @?=
    Right (String' "ي الخاطفة الجديدة، مع, بلديهم")
  , testCase "string with japanese characters" $
    parseSexp' "\"媯綩 づ竤バ り姥娩ぎょひ\"" @?=
    Right (String' "媯綩 づ竤バ り姥娩ぎょひ")
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
    (Left ("<no location information>:1:0: mismatch:\n  expected: bool\n       got: 10") :: Either String (Foo Bool Bool))
  ]

testArithExpr :: ArithExpr
testArithExpr = Add (Lit 0) (Mul [])

testArithExprSexp :: Sexp
testArithExprSexp = List' [Symbol' "+", Int' 0, List' [Symbol' "*"]]

parseTests :: TestTree
parseTests = testGroup "parse tests"
  [ testCase "(+ 0 (*))" $
      Right testArithExpr @=? G.parseSexp arithExprGenericIso testArithExprSexp
  ]

genTests :: TestTree
genTests = testGroup "gen tests"
  [ testCase "(+ 0 (*))" $
      Right testArithExprSexp @=? G.genSexp arithExprGenericIso testArithExpr
  ]


genParseIdentityProp :: forall a. (Eq a) => (forall t. Grammar Position (Sexp :- t) (a :- t)) -> a -> Bool
genParseIdentityProp iso expr =
  (G.genSexp iso expr >>= G.parseSexp iso :: Either String a)
  ==
  Right expr

parseGenTests :: TestTree
parseGenTests = testGroup "parse . gen == id"
  [ QC.testProperty "ArithExprs/TH" (genParseIdentityProp arithExprTHIso)
  , QC.testProperty "ArithExprs/Generics" (genParseIdentityProp arithExprGenericIso)
  , QC.testProperty "Pair Int String" (genParseIdentityProp (pairGenericIso int string'))
  , QC.testProperty "Foo (Foo Int String) (Pair String Int)" (genParseIdentityProp (fooGenericIso (fooGenericIso int string') (pairGenericIso string' int)))
  , QC.testProperty "Person" (genParseIdentityProp personGenericIso)
  ]

main :: IO ()
main = defaultMain allTests
