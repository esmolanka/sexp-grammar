{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE RankNTypes           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Prelude hiding ((.), id)
import Control.Category
import Data.Data (Data)
import Data.Text.Lazy (Text)
import Language.Sexp
import Language.SexpGrammar

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

data Prim
  = SquareRoot
  | Factorial
  | Fibonacci
    deriving (Eq, Enum, Bounded, Data, Show)

return []

instance SexpIso Prim

instance SexpIso Ident where
  sexpIso = $(grammarFor 'Ident) . symbol'

instance SexpIso Expr where
  sexpIso = coproduct
    [ $(grammarFor 'Var) . sexpIso
    , $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Inv) . list (el (sym "invert") >>> el sexpIso)
    , $(grammarFor 'IfZero) . list (el (sym "cond") >>> props ( Kw "pred"  .: sexpIso
                                                            >>> Kw "true"  .: sexpIso
                                                            >>> Kw "false" .: sexpIso ))
    , $(grammarFor 'Apply) .              -- Convert prim :- "dummy" :- args to Apply node
        list
         (el (sexpIso :: SexpG Prim) >>>       -- Push prim: prim :- ()
          el (kw (Kw "args")) >>>              -- Recognize :args, push nothing
          rest (sexpIso :: SexpG Expr) >>>     -- Push args: args :- prim :- ()
          swap >>>                             -- Swap: prim :- args :- ()
          push "dummy" >>>                     -- Push "dummy" :- "dummy" :- prim :- args
          swap                                 -- Swap: prim :- "dummy" :- args
         )
    ]

exprGrammar :: SexpG Expr
exprGrammar = sexpIso

test :: String -> SexpG a -> (a, Text)
test str g = either error id $ do
  e <- parseFromString g str
  sexp' <- gen g e
  return (e, printSexp sexp')

-- > test "(cond 1 (+ 42 10) (* 2 (* 2 2)))"
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Mul (Lit 2) (Mul (Lit 2) (Lit 2))),"(cond 1 (+ 42 10) (* 2 (* 2 2)))")

