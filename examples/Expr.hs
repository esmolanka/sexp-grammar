{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Prelude hiding ((.), id)
import Control.Category
import Data.Data (Data)
import qualified Data.ByteString.Lazy.Char8 as B8

import qualified Language.Sexp as Sexp
import Language.SexpGrammar
import Language.SexpGrammar.Generic
import GHC.Generics

newtype Ident = Ident String
  deriving (Show, Generic)

data Expr
  = Var Ident
  | Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Inv Expr
  | IfZero Expr Expr Expr
  | Apply [Expr] String Prim -- inconvenient ordering: arguments, useless annotation, identifier
    deriving (Show, Generic)

data Prim
  = SquareRoot
  | Factorial
  | Fibonacci
    deriving (Eq, Enum, Bounded, Data, Show, Generic)

instance SexpIso Prim

instance SexpIso Ident where
  sexpIso = with symbol'

instance SexpIso Expr where
  sexpIso = match
    $ With (\var -> var . sexpIso)
    $ With (\lit -> lit . int)
    $ With (\add -> add . list (el (sym "+") >>> el sexpIso >>> el sexpIso))
    $ With (\mul -> mul . list (el (sym "*") >>> el sexpIso >>> el sexpIso))
    $ With (\neg -> neg . list (el (sym "negate") >>> el sexpIso))
    $ With (\inv -> inv . list (el (sym "invert") >>> el sexpIso))
    $ With (\ifz -> ifz . list (el (sym "cond") >>> props ( Kw "pred"  .: sexpIso
                                          >>> Kw "true"  .: sexpIso
                                          >>> Kw "false" .: sexpIso )))
    $ With (\app -> app . list
        (el (sexpIso :: SexpG Prim) >>>       -- Push prim:       prim :- ()
         el (kw (Kw "args")) >>>              -- Recognize :args, push nothing
         rest (sexpIso :: SexpG Expr) >>>     -- Push args:       args :- prim :- ()
         swap >>>                             -- Swap:            prim :- args :- ()
         push "dummy" >>>                     -- Push "dummy":    "dummy" :- prim :- args :- ()
         swap                                 -- Swap:            prim :- "dummy" :- args :- ()
        ))
    $ End

exprGrammar :: SexpG Expr
exprGrammar = sexpIso

test :: String -> SexpG a -> (a, String)
test str g = either error id $ do
  e <- decodeWith g (B8.pack str)
  sexp' <- genSexp g e
  return (e, B8.unpack (Sexp.encode sexp'))

-- > test "(cond 1 (+ 42 10) (* 2 (* 2 2)))"
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Mul (Lit 2) (Mul (Lit 2) (Lit 2))),"(cond 1 (+ 42 10) (* 2 (* 2 2)))")


