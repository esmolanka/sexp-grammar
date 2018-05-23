{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ExprTH2 where

import Prelude hiding ((.), id)

import Control.Category
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Data (Data)
import Data.Text (Text)
import qualified Language.Sexp.Located as Sexp
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
  sexpIso = $(match ''Prim)
    (sym "square-root" >>>)
    (sym "factorial" >>>)
    (sym "fibonacci" >>>)

instance SexpIso Ident where
  sexpIso = $(match ''Ident)
    (\_Ident -> _Ident . symbol)

instance SexpIso Expr where
  sexpIso = $(match ''Expr)
    (\_Var -> _Var . sexpIso)
    (\_Lit -> _Lit . int)
    (\_Add -> _Add . list (el (sym "+") >>> el sexpIso >>> el sexpIso))
    (\_Mul -> _Mul . list (el (sym "*") >>> el sexpIso >>> el sexpIso))
    (\_Inv -> _Inv . list (el (sym "invert") >>> el sexpIso))
    (\_IfZero -> _IfZero . list (el (sym "cond") >>> props ( "pred"  .:  sexpIso
                                                         >>> "true"  .:  sexpIso
                                                         >>> "false" .:? sexpIso )))
    (\_Apply -> _Apply .              -- Convert prim :- "dummy" :- args :- () to Apply node
        list
         (el (sexpIso :: SexpGrammar Prim) >>>   -- Push prim:       prim :- ()
          el (kwd "args") >>>                    -- Recognize :args, push nothing
          rest (sexpIso :: SexpGrammar Expr) >>> -- Push args:       args :- prim :- ()
          onTail (
             swap >>>                            -- Swap:            prim :- args :- ()
             push "dummy" (const True) >>>       -- Push "dummy":    "dummy" :- prim :- args :- ()
             swap)                               -- Swap:            prim :- "dummy" :- args :- ()
         ))

test :: String -> SexpGrammar a -> (a, String)
test str g = either error id $ do
  e <- decodeWith g "<stdin>" (B8.pack str)
  sexp' <- toSexp g e
  return (e, B8.unpack (Sexp.format sexp'))

-- λ> test "(cond :pred 1 :true (+ 42 10) :false (* 2 (* 2 2)))" (sexpIso :: SexpG Expr)
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Just (Mul (Lit 2) (Mul (Lit 2) (Lit 2)))),"(cond :false (* 2 (* 2 2)) :pred 1 :true (+ 42 10))")
