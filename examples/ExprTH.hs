{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ExprTH where

import Prelude hiding ((.), id)

import Control.Category
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Data (Data)
import qualified Language.Sexp as Sexp
import Language.SexpGrammar
import Language.SexpGrammar.TH

newtype Ident = Ident String
  deriving (Show)

data Expr
  = Var Ident
  | Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Inv Expr
  | IfZero Expr Expr (Maybe Expr)
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
    , $(grammarFor 'IfZero) . list (el (sym "cond") >>> props ( Kw "pred"  .:  sexpIso
                                                            >>> Kw "true"  .:  sexpIso
                                                            >>> Kw "false" .:? sexpIso ))
    , $(grammarFor 'Apply) .              -- Convert prim :- "dummy" :- args :- () to Apply node
        list
         (el (sexpIso :: SexpG Prim) >>>       -- Push prim:       prim :- ()
          el (kw (Kw "args")) >>>              -- Recognize :args, push nothing
          rest (sexpIso :: SexpG Expr) >>>     -- Push args:       args :- prim :- ()
          swap >>>                             -- Swap:            prim :- args :- ()
          push "dummy" >>>                     -- Push "dummy":    "dummy" :- prim :- args :- ()
          swap                                 -- Swap:            prim :- "dummy" :- args :- ()
         )
    ]

test :: String -> SexpG a -> (a, String)
test str g = either error id $ do
  e <- decodeWith g (B8.pack str)
  sexp' <- genSexp g e
  return (e, B8.unpack (Sexp.encode sexp'))

-- λ> test "(cond :pred 1 :true (+ 42 10) :false (* 2 (* 2 2)))" (sexpIso :: SexpG Expr)
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Just (Mul (Lit 2) (Mul (Lit 2) (Lit 2)))),"(cond :false (* 2 (* 2 2)) :pred 1 :true (+ 42 10))")
