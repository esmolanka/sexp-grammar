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
import Data.Scientific
import Data.Semigroup
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as TS
import GHC.Generics
import Test.QuickCheck ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Language.Sexp as Sexp
import Language.SexpGrammar as G
import Language.SexpGrammar.Generic
import Language.SexpGrammar.TH hiding (match)

parseSexp' :: String -> Either String BareSexp
parseSexp' input = Sexp.decode (fromString input)

data Pair a b = Pair a b
  deriving (Show, Eq, Ord, Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

data Foo a b
  = Bar a b
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

string' :: Grammar Position (Sexp :- t) (String :- t)
string' = string >>> iso TS.unpack TS.pack

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
       ":age"     .: int >>>
       ":address" .: string') >>>
      rest personGenericIso) >>> person)


----------------------------------------------------------------------
-- Test cases

allTests :: TestTree
allTests = testGroup "All tests"
  [ lexerTests
  , grammarTests
  ]

(@?=~) :: Either String BareSexp -> Either String Sexp -> Assertion
(@?=~) a b = a @?= fmap stripLocation b

lexerTests :: TestTree
lexerTests = testGroup "Sexp lexer/parser tests"
  [ testCase "123 is an integer number" $
      parseSexp' "123"
      @?=~ Right (Number 123)
  , testCase "+123 is an integer number" $
      parseSexp' "+123"
      @?=~ Right (Number 123)
  , testCase "-123 is an integer number" $
      parseSexp' "-123"
      @?=~ Right (Number (- 123))
  , testCase "+123.4e5 is a floating number" $
      parseSexp' "+123.4e5"
      @?=~ Right (Number (read "+123.4e5" :: Scientific))
  , testCase "comments" $
      parseSexp' ";; hello, world\n   123"
      @?=~ Right (Number 123)
  , testCase "cyrillic characters in comments" $
      parseSexp' ";; привет!\n   123"
      @?=~ Right (Number 123)
  , testCase "unicode math in comments" $
      parseSexp' ";; Γ ctx\n;; ----- Nat-formation\n;; Γ ⊦ Nat : Type\nfoobar"
      @?=~ Right (Symbol "foobar")
  , testCase "symbol" $
      parseSexp' "hello-world"
      @?=~ Right (Symbol "hello-world")
  , testCase "whitespace and symbol" $
      parseSexp' "\t\n   hello-world\n"
      @?=~ Right (Symbol "hello-world")
  , testCase "cyrillic symbol" $
      parseSexp' "символ"
      @?=~ Right (Symbol "символ")
  , testCase "string with arabic characters" $
      parseSexp' "\"ي الخاطفة الجديدة، مع, بلديهم\""
      @?=~ Right (String "ي الخاطفة الجديدة، مع, بلديهم")
  , testCase "string with japanese characters" $
      parseSexp' "\"媯綩 づ竤バ り姥娩ぎょひ\""
      @?=~ Right (String "媯綩 づ竤バ り姥娩ぎょひ")
  , testCase "paren-list" $
      parseSexp' "(foo bar)"
      @?=~ Right (ParenList [Symbol "foo", Symbol "bar"])
  , testCase "bracket-list" $
      parseSexp' "[foo bar]"
      @?=~ Right (BracketList [Symbol "foo", Symbol "bar"])
  , testCase "brace-list" $
      parseSexp' "{foo bar}"
      @?=~ Right (BraceList [Symbol "foo", Symbol "bar"])
  , testCase "quoted" $
      parseSexp' "'foo"
      @?=~ Right (Quoted (Symbol "foo"))
  , testCase "hashed" $
      parseSexp' "#foo"
      @?=~ Right (Symbol "#foo")
  , testCase "keyword" $
      parseSexp' ":foo"
      @?=~ Right (Symbol ":foo")
  ]


grammarTests :: TestTree
grammarTests = testGroup "Grammar tests"
  [ baseTypeTests
  , listTests
  , dictTests
  , revStackPrismTests
  , parseTests
  , genTests
  , parseGenTests
  ]


baseTypeTests :: TestTree
baseTypeTests = testGroup "Base type combinator tests"
  [ testCase "bool/true" $
    G.fromSexp bool (Symbol "tt") @?= Right True

  , testCase "bool/false" $
    G.fromSexp bool (Symbol "ff") @?= Right False

  , testCase "integer" $
    G.fromSexp integer (Number (42 ^ (42 :: Integer))) @?= Right (42 ^ (42 :: Integer))

  , testCase "int" $
    G.fromSexp int (Number 65536) @?= Right 65536

  , testCase "real" $
    G.fromSexp real (Number  3.14) @?= Right 3.14

  , testCase "double" $
    G.fromSexp double (Number  3.14) @?= Right 3.14

  , testCase "string" $
    G.fromSexp string (String "foo\nbar baz") @?= Right "foo\nbar baz"

  , testCase "string'" $
    G.fromSexp string' (String "foo\nbar baz") @?= Right "foo\nbar baz"

  , testCase "symbol" $
    G.fromSexp symbol (Symbol "foobarbaz") @?= Right "foobarbaz"

  ]


listTests :: TestTree
listTests = testGroup "List combinator tests"
  [ testCase "empty list of bools" $
    G.fromSexp (list (rest bool)) (ParenList []) @?= Right []

  , testCase "list of bools" $
    G.fromSexp (list (rest bool)) (ParenList [Symbol "tt", Symbol "ff", Symbol "ff"]) @?=
    Right [True, False, False]

  , testCase "vector of ints" $
    G.fromSexp (vect (rest int)) (BracketList [Number 123, Number 0, Number (-100)]) @?=
    Right [123, 0, -100]

  , testCase "brace-list of strings" $
    G.fromSexp (bracelist (rest string)) (BraceList [String "foo", String "bar"]) @?=
    Right ["foo", "bar"]
  ]


dictTests :: TestTree
dictTests = testGroup "Dict combinator tests"
  [ testCase "simple dict, present key" $
    G.fromSexp (dict (key "foo" int)) (BraceList [Symbol ":foo", Number 42]) @?=
    Right 42

  , testCase "simple dict, missing key" $
    G.fromSexp (dict (key "bar" int)) (BraceList [Symbol ":foo", Number 42]) @?=
    (Left ("<no location information>:1:0: mismatch:\n    Expected: keyword :bar") :: Either String Int)

  , testCase "simple dict, missing optional key" $
    G.fromSexp (dict (keyMay "bar" int)) (BraceList []) @?=
    Right Nothing

  , testCase "simple dict, extra key" $
    G.fromSexp (dict (key "foo" int)) (BraceList [Symbol ":foo", Number 42, Symbol ":bar", Number 0]) @?=
    (Left ("<no location information>:1:0: mismatch:\n    Unexpected: keyword :bar") :: Either String Int)

  ]


revStackPrismTests :: TestTree
revStackPrismTests = testGroup "Reverse stack prism tests"
  [ testCase "pair of two bools" $
    G.fromSexp sexpIso (ParenList [Symbol "ff", Symbol "tt"]) @?=
    Right (Pair False True)

  , testCase "sum of products (Bar True 42)" $
    G.fromSexp sexpIso (ParenList [Symbol "bar", Symbol "tt", Number 42]) @?=
    Right (Bar True (42 :: Int))

  , testCase "sum of products (Baz True False) tries to parse (baz #f 10)" $
    G.fromSexp sexpIso (ParenList [Symbol "baz", Symbol "ff", Number 10]) @?=
    (Left ("<no location information>:1:0: mismatch:\n    Expected: bool\n    But got:  10") :: Either String (Foo Bool Bool))
  ]


testArithExpr :: ArithExpr
testArithExpr =
  Add (Lit 0) (Mul [])

testArithExprSexp :: Sexp
testArithExprSexp =
  ParenList [Symbol "+", Number 0, ParenList [Symbol "*"]]


parseTests :: TestTree
parseTests = testGroup "parse tests"
  [ testCase "(+ 0 (*))" $
      G.fromSexp arithExprGenericIso testArithExprSexp
      @?= Right testArithExpr
  ]


genTests :: TestTree
genTests = testGroup "gen tests"
  [ testCase "(+ 0 (*))" $
      G.toSexp arithExprGenericIso testArithExpr
      @?= Right testArithExprSexp
  ]


genParseIdentityProp :: forall a. (Eq a) => (forall t. Grammar Position (Sexp :- t) (a :- t)) -> a -> Bool
genParseIdentityProp iso expr =
  (G.toSexp iso expr >>= G.fromSexp iso :: Either String a)
  ==
  Right expr


parseGenTests :: TestTree
parseGenTests = testGroup "parse . gen == id"
  [ QC.testProperty "ArithExprs/TH" $
      genParseIdentityProp arithExprTHIso

  , QC.testProperty "ArithExprs/Generics" $
      genParseIdentityProp arithExprGenericIso

  , QC.testProperty "Pair Int String" $
      genParseIdentityProp (pairGenericIso int string')

  , QC.testProperty "Foo (Foo Int String) (Pair String Int)" $
      genParseIdentityProp (fooGenericIso (fooGenericIso int string') (pairGenericIso string' int))

  , QC.testProperty "Person" $
      genParseIdentityProp personGenericIso
  ]


main :: IO ()
main = defaultMain allTests
