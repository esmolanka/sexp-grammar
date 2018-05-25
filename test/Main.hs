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
import qualified Data.Set as S
import GHC.Generics
import Test.QuickCheck ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Text.Prettyprint.Doc (Pretty, pretty)

import Language.Sexp.Located as Sexp
import Language.Sexp () -- for Show instance

import Data.InvertibleGrammar (ErrorMessage(..), runGrammar, forward, backward)

import Language.SexpGrammar as G
import Language.SexpGrammar.Generic
import Language.SexpGrammar.TH hiding (match)

parseSexp' :: String -> Either String Sexp
parseSexp' input = Sexp.decode (fromString input)

fromSexp' :: SexpGrammar a -> Sexp.Sexp -> Either (ErrorMessage Position) a
fromSexp' g = runGrammar Sexp.dummyPos . forward (G.sealed g)

toSexp' :: SexpGrammar a -> a -> Either (ErrorMessage Position) Sexp.Sexp
toSexp' g = runGrammar Sexp.dummyPos . backward (G.sealed g)

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
  { _pName     :: String
  , _pAge      :: Int
  , _pAddress  :: String
  , _pChildren :: [Person]
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

sexpEq :: (Pretty e, Eq e) => Either e Sexp -> Either e Sexp -> Assertion
sexpEq a b =
  fmap toSimple a `otherEq` fmap toSimple b

otherEq :: (Pretty e, Eq e, Show a, Eq a) => Either e a -> Either e a -> Assertion
otherEq a b = do
  (flip assertBool) (a == b) $
    unlines
      ["Output mismatch:"
      , ppOutput a
      , "vs."
      , ppOutput b
      ]
  where
    ppOutput o = case o of
      Left err -> "Error message: " ++ show (pretty err)
      Right v  -> "Output: " ++ show v

lexerTests :: TestTree
lexerTests = testGroup "Sexp lexer/parser tests"
  [ testCase "123 is an integer number" $
      parseSexp' "123"
      `sexpEq` Right (Number 123)
  , testCase "+123 is an integer number" $
      parseSexp' "+123"
      `sexpEq` Right (Number 123)
  , testCase "-123 is an integer number" $
      parseSexp' "-123"
      `sexpEq` Right (Number (- 123))
  , testCase "+123.4e5 is a floating number" $
      parseSexp' "+123.4e5"
      `sexpEq` Right (Number (read "+123.4e5" :: Scientific))
  , testCase "comments" $
      parseSexp' ";; hello, world\n   123"
      `sexpEq` Right (Number 123)
  , testCase "cyrillic characters in comments" $
      parseSexp' ";; привет!\n   123"
      `sexpEq` Right (Number 123)
  , testCase "unicode math in comments" $
      parseSexp' ";; Γ ctx\n;; ----- Nat-formation\n;; Γ ⊦ Nat : Type\nfoobar"
      `sexpEq` Right (Symbol "foobar")
  , testCase "symbol" $
      parseSexp' "hello-world"
      `sexpEq` Right (Symbol "hello-world")
  , testCase "whitespace and symbol" $
      parseSexp' "\t\n   hello-world\n"
      `sexpEq` Right (Symbol "hello-world")
  , testCase "cyrillic symbol" $
      parseSexp' "символ"
      `sexpEq` Right (Symbol "символ")
  , testCase "string with arabic characters" $
      parseSexp' "\"ي الخاطفة الجديدة، مع, بلديهم\""
      `sexpEq` Right (String "ي الخاطفة الجديدة، مع, بلديهم")
  , testCase "string with japanese characters" $
      parseSexp' "\"媯綩 づ竤バ り姥娩ぎょひ\""
      `sexpEq` Right (String "媯綩 づ竤バ り姥娩ぎょひ")
  , testCase "paren-list" $
      parseSexp' "(foo bar)"
      `sexpEq` Right (ParenList [Symbol "foo", Symbol "bar"])
  , testCase "bracket-list" $
      parseSexp' "[foo bar]"
      `sexpEq` Right (BracketList [Symbol "foo", Symbol "bar"])
  , testCase "brace-list" $
      parseSexp' "{foo bar}"
      `sexpEq` Right (BraceList [Symbol "foo", Symbol "bar"])
  , testCase "quoted" $
      parseSexp' "'foo"
      `sexpEq` Right (Modified Quote (Symbol "foo"))
  , testCase "hashed" $
      parseSexp' "#foo"
      `sexpEq` Right (Symbol "#foo")
  , testCase "keyword" $
      parseSexp' ":foo"
      `sexpEq` Right (Symbol ":foo")
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
    fromSexp' sexpIso (Symbol "true") `otherEq`
    Right True

  , testCase "bool/false" $
    fromSexp' sexpIso (Symbol "false") `otherEq`
    Right False

  , testCase "integer" $
    fromSexp' integer (Number (42 ^ (42 :: Integer))) `otherEq`
    Right (42 ^ (42 :: Integer))

  , testCase "int" $
    fromSexp' int (Number 65536) `otherEq`
    Right 65536

  , testCase "real" $
    fromSexp' real (Number  3.14) `otherEq`
    Right 3.14

  , testCase "double" $
    fromSexp' double (Number  3.14) `otherEq`
    Right 3.14

  , testCase "string" $
    fromSexp' string (String "foo\nbar baz") `otherEq`
    Right "foo\nbar baz"

  , testCase "string'" $
    fromSexp' string' (String "foo\nbar baz") `otherEq`
    Right "foo\nbar baz"

  , testCase "symbol" $
    fromSexp' symbol (Symbol "foobarbaz") `otherEq`
    Right "foobarbaz"
  ]


listTests :: TestTree
listTests = testGroup "List combinator tests"
  [ testCase "empty list of ints" $
    fromSexp'
      (list (rest int))
      (ParenList []) `otherEq`
    Right []

  , testCase "list of strings" $
    fromSexp'
      (list (rest string))
      (ParenList [String "tt", String "ff", String "ff"]) `otherEq`
    Right ["tt", "ff", "ff"]

  , testCase "bracket list of ints" $
    fromSexp'
      (bracketList (rest int))
      (BracketList [Number 123, Number 0, Number (-100)]) `otherEq`
    Right [123, 0, -100]

  , testCase "brace list of strings" $
    fromSexp'
      (braceList (rest string))
      (BraceList [String "foo", String "bar"]) `otherEq`
    Right ["foo", "bar"]
  ]


dictTests :: TestTree
dictTests = testGroup "Dict combinator tests"
  [ testCase "simple dict, present key" $
    fromSexp'
      (braceList (props (key "foo" int)))
      (BraceList [Symbol ":foo", Number 42]) `otherEq`
    Right 42

  , testCase "simple dict, missing key" $
    fromSexp'
      (braceList (props (key "bar" int)))
      (BraceList [Symbol ":foo", Number 42]) `otherEq`
    (Left (ErrorMessage dummyPos [] (S.fromList ["keyword :bar"]) Nothing))

  , testCase "simple dict, missing optional key" $
    fromSexp'
      (braceList (props (optKey "bar" int)))
      (BraceList []) `otherEq`
    Right Nothing

  , testCase "simple dict, extra key" $
    fromSexp'
      (braceList (props (key "foo" int)))
      (BraceList [Symbol ":foo", Number 42, Symbol ":bar", Number 0]) `otherEq`
    (Left (ErrorMessage dummyPos [] mempty (Just "keyword :bar")))

  , testCase "simple dict, remaining keys, from" $
    fromSexp'
      (braceList (props (restKeys (int >>> pair))))
      (BraceList [Symbol ":foo", Number 42, Symbol ":bar", Number 0]) `otherEq`
    (Right [("foo", 42), ("bar", 0)])

  , testCase "simple dict, remaining keys, to" $
    toSexp'
      (braceList (props (restKeys (int >>> pair))))
      [("foo", 42), ("bar", 0)]  `sexpEq`
    (Right (BraceList [Symbol ":foo", Number 42, Symbol ":bar", Number 0]))

  , testCase "simple dict, remaining keys then one more" $
    fromSexp'
      (braceList (props (restKeys (int >>> pair) >>> key "baz" int)) >>> pair)
      (BraceList [Symbol ":foo", Number 42, Symbol ":bar", Number 0]) `otherEq`
    (Left (ErrorMessage dummyPos [] (S.fromList ["keyword :baz"]) Nothing))
  ]


revStackPrismTests :: TestTree
revStackPrismTests = testGroup "Reverse stack prism tests"
  [ testCase "pair of two bools" $
    fromSexp' sexpIso (ParenList [Symbol "false", Symbol "true"]) `otherEq`
    Right (Pair False True)

  , testCase "sum of products (Bar True 42)" $
    fromSexp' sexpIso (ParenList [Symbol "bar", Symbol "true", Number 42]) `otherEq`
    Right (Bar True (42 :: Int))

  , testCase "sum of products (Baz True False) tries to parse (baz #f 10)" $
    fromSexp' (sexpIso :: SexpGrammar (Foo Bool Bool))
    (ParenList [Symbol "baz", Symbol "false", Number 10]) `otherEq`
    (Left (ErrorMessage dummyPos [] (S.fromList ["symbol false", "symbol true"]) (Just "10")))
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
      fromSexp' arithExprGenericIso testArithExprSexp
      `otherEq` Right testArithExpr
  ]


genTests :: TestTree
genTests = testGroup "gen tests"
  [ testCase "(+ 0 (*))" $
      toSexp' arithExprGenericIso testArithExpr
      `otherEq` Right testArithExprSexp
  ]


genParseIdentityProp :: forall a. (Eq a) => (forall t. Grammar Position (Sexp :- t) (a :- t)) -> a -> Bool
genParseIdentityProp iso expr =
  (toSexp' iso expr >>= fromSexp' iso :: Either (ErrorMessage Position) a)
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
