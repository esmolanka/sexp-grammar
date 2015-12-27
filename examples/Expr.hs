{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Prelude hiding ((.), id)
import Control.Category
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
    deriving (Show)

return []

instance SexpIso a => SexpIso (Maybe a) where
  sexpIso = sconcat
    [ $(grammarFor 'Nothing) . sym "nil"
    , $(grammarFor 'Just) . list (el (sym "just") >>> el sexpIso)
    ]

instance SexpIso Ident where
  sexpIso = $(grammarFor 'Ident) . symbol'

instance SexpIso Expr where
  sexpIso = sconcat
    [ $(grammarFor 'Var) . sexpIso
    , $(grammarFor 'Lit) . int
    , $(grammarFor 'Add) . list (el (sym "+") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Mul) . list (el (sym "*") >>> el sexpIso >>> el sexpIso)
    , $(grammarFor 'Inv) . list (el (sym "invert") >>> el sexpIso)
    , $(grammarFor 'IfZero) . list (el (sym "cond")
                                    >>> el sexpIso
                                    >>> el sexpIso
                                    >>> el sexpIso)
    ]

test :: String -> (Expr, Text)
test str = either error id $ do
  sexp <- parseSexp "<input>" str
  expr <- parse sexpIso sexp
  sexp' <- gen sexpIso expr
  return (expr, printSexp sexp')

-- > test "(cond 1 (+ 42 10) (* 2 (* 2 2)))"
-- (IfZero (Lit 1) (Add (Lit 42) (Lit 10)) (Mul (Lit 2) (Mul (Lit 2) (Lit 2))),"(cond 1 (+ 42 10) (* 2 (* 2 2)))")
