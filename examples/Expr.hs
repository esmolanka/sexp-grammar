{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE RankNTypes           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Prelude hiding ((.), id)
import Control.Category
import Data.Data (Data)
import Data.Semigroup
import Data.Text.Lazy (Text)
import Language.Sexp
import Language.SexpGrammar

import Debug.Trace (trace)

traceIt :: Show a => a -> a
traceIt a = trace ("It: " ++ show a) a

newtype Ident = Ident String
  deriving (Show)

data Expr
  = Var Ident
  | Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Inv Expr
  | IfZero Expr Expr Expr
  | Apply [Expr] String Prim -- inconvenient ordering: arguments, useless annotation, identifier
    deriving (Show)

data Pair a b = Pair a b deriving (Show)

data Prim
  = SquareRoot
  | Factorial
  | Fibonacci
    deriving (Eq, Enum, Bounded, Data, Show)

return []

instance SexpIso Prim

instance (SexpIso a, SexpIso b) => SexpIso (Pair a b) where
  sexpIso =
    list (              -- begin list
      el sexpIso >>>    -- consume and push first element to stack:  (a :- t)
      el sexpIso        -- consume and push second element to stack: (b :- a :- t)
    ) >>>
    $(grammarFor 'Pair) -- pop b, pop a, apply a to Pair,
                        -- apply b to (Pair a):                      (Pair a b :- t)

instance SexpIso Ident where
  sexpIso = $(grammarFor 'Ident) . symbol'

instance SexpIso Expr where
  sexpIso = sconcat
    [ $(grammarFor 'Var) . sexpIso
    , $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Inv) . list (el (sym "invert") >>> el sexpIso)
    , $(grammarFor 'IfZero) . list (el (sym "cond") >>> props ( Kw "pred"  .: sexpIso
                                                            >>> Kw "true"  .: sexpIso
                                                            >>> Kw "false" .: sexpIso ))
    , $(grammarFor 'Apply) . list
         (el (sexpIso :: SexpG Prim) >>>
          el (kw (Kw "args")) >>>
          rest (sexpIso :: SexpG Expr) >>>
          swap >>>
          push "dummy" >>>
          swap
         )
    ]

sexp :: String -> Sexp
sexp = either error id . parseSexp "<inline>"

test :: String -> SexpG a -> (a, Text)
test str g = either error id $ do
  sexp <- parseSexp "<input>" str
  expr <- parse g sexp
  sexp' <- gen g expr
  return (expr, printSexp sexp')

-- > test "(cond 1 (+ 42 10) (* 2 (* 2 2)))"
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Mul (Lit 2) (Mul (Lit 2) (Lit 2))),"(cond 1 (+ 42 10) (* 2 (* 2 2)))")

