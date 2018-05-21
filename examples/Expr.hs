{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expr where

import Prelude hiding ((.), id)
import Control.Category
import Data.Data (Data)
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Text (Text)
import qualified Data.Text.Lazy as T

import qualified Language.Sexp as Sexp
import Language.SexpGrammar
import Language.SexpGrammar.Generic
import GHC.Generics

newtype Ident = Ident Text
  deriving (Show, Generic)

data Expr
  = Var Ident
  | Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Inv Expr
  | IfZero Expr Expr (Maybe Expr)
  | Apply [Expr] String Prim -- inconvenient ordering: arguments, useless annotation, identifier
    deriving (Show, Generic)

data Prim
  = SquareRoot
  | Factorial
  | Fibonacci
    deriving (Eq, Enum, Bounded, Data, Show, Generic)

instance SexpIso Prim where
  sexpIso = enum

instance SexpIso Ident where
  sexpIso = with (\ident -> ident . symbol)

instance SexpIso Expr where
  sexpIso = match
    $ With (\var -> var . sexpIso)
    $ With (\lit -> lit . int)
    $ With (\add -> add . list (el (sym "+") >>> el sexpIso >>> el sexpIso))
    $ With (\mul -> mul . list (el (sym "*") >>> el sexpIso >>> el sexpIso))
    $ With (\neg -> neg . list (el (sym "negate") >>> el sexpIso))
    $ With (\inv -> inv . list (el (sym "invert") >>> el sexpIso))
    $ With (\ifz -> ifz . list (el (sym "cond") >>> props ( "pred"  .: sexpIso
                                                        >>> "true"  .:  sexpIso
                                                        >>> "false" .:? sexpIso )))
    $ With (\app -> app . list
        (el (sexpIso :: SexpGrammar Prim) >>>   -- Push prim:       prim :- ()
         el (kwd "args") >>>                    -- Recognize :args, push nothing
         rest (sexpIso :: SexpGrammar Expr) >>> -- Push args:       args :- prim :- ()
         onTail (swap >>> push "dummy" (const True) >>> swap)
        ))
    $ End

exprGrammar :: SexpGrammar Expr
exprGrammar = sexpIso

test :: String -> SexpGrammar a -> (a, String)
test str g = either error id $ do
  e <- decodeWith g (B8.pack str)
  sexp' <- toSexp g e
  return (e, T.unpack (Sexp.prettySexp sexp'))

-- > test "(cond 1 (+ 42 10) (* 2 (* 2 2)))"
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Mul (Lit 2) (Mul (Lit 2) (Lit 2))),"(cond 1 (+ 42 10) (* 2 (* 2 2)))")
