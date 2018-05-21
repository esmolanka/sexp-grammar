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
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Language.Sexp as Sexp
import Language.SexpGrammar
import Language.SexpGrammar.TH

newtype Ident = Ident Text
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

instance SexpIso Prim where
  sexpIso = enum

instance SexpIso Ident where
  sexpIso = $(grammarFor 'Ident) . symbol

instance SexpIso Expr where
  sexpIso = coproduct
    [ $(grammarFor 'Var) . sexpIso
    , $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Inv) . list (el (sym "invert") >>> el sexpIso)
    , $(grammarFor 'IfZero) . list (el (sym "cond") >>> props ( "pred"  .:  sexpIso
                                                            >>> "true"  .:  sexpIso
                                                            >>> "false" .:? sexpIso ))
    , $(grammarFor 'Apply) .              -- Convert prim :- "dummy" :- args :- () to Apply node
        list
         (el (sexpIso :: SexpGrammar Prim) >>>   -- Push prim:       prim :- ()
          el (kwd "args") >>>                -- Recognize :args, push nothing
          rest (sexpIso :: SexpGrammar Expr) >>> -- Push args:       args :- prim :- ()
          onTail (
             swap >>>                            -- Swap:            prim :- args :- ()
             push "dummy" (const True) >>>       -- Push "dummy":    "dummy" :- prim :- args :- ()
             swap)                               -- Swap:            prim :- "dummy" :- args :- ()
         )
    ]

test :: String -> SexpGrammar a -> (a, String)
test str g = either error id $ do
  e <- decodeWith g (B8.pack str)
  sexp' <- toSexp g e
  return (e, T.unpack (Sexp.prettySexp sexp'))

-- λ> test "(cond :pred 1 :true (+ 42 10) :false (* 2 (* 2 2)))" (sexpIso :: SexpG Expr)
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Just (Mul (Lit 2) (Mul (Lit 2) (Lit 2)))),"(cond :false (* 2 (* 2 2)) :pred 1 :true (+ 42 10))")
