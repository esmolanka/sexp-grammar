{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

import Criterion.Main

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Data.Data (Data, Typeable)
import qualified Data.Text.Lazy as TL
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
    deriving (Show, Eq, Enum, Bounded, Data, Typeable)

return []

instance SexpIso Prim

instance SexpIso Ident where
  sexpIso = $(match ''Ident)
    (\_Ident -> _Ident . symbol')

instance SexpIso Expr where
  sexpIso = $(match ''Expr)
    (\_Var -> _Var . sexpIso)
    (\_Lit -> _Lit . int)
    (\_Add -> _Add . list (el (sym "+") >>> el sexpIso >>> el sexpIso))
    (\_Mul -> _Mul . list (el (sym "*") >>> el sexpIso >>> el sexpIso))
    (\_Inv -> _Inv . list (el (sym "invert") >>> el sexpIso))
    (\_IfZero -> _IfZero . list (el (sym "cond") >>> props ( Kw "pred"  .:  sexpIso
                                                         >>> Kw "true"  .:  sexpIso
                                                         >>> Kw "false" .:? sexpIso )))
    (\_Apply -> _Apply .              -- Convert prim :- "dummy" :- args :- () to Apply node
        list
         (el (sexpIso :: SexpG Prim) >>>       -- Push prim:       prim :- ()
          el (kw (Kw "args")) >>>              -- Recognize :args, push nothing
          rest (sexpIso :: SexpG Expr) >>>     -- Push args:       args :- prim :- ()
          swap >>>                             -- Swap:            prim :- args :- ()
          push "dummy" >>>                     -- Push "dummy":    "dummy" :- prim :- args :- ()
          swap                                 -- Swap:            prim :- "dummy" :- args :- ()
         ))


parseExpr :: Sexp -> Either String Expr
parseExpr = parseSexp sexpIso

genExpr :: Expr -> Either String Sexp
genExpr = genSexp sexpIso

expr :: TL.Text -> Expr
expr = either error id . decode

benchCases :: [(String, TL.Text)]
benchCases = map (\a -> ("expression " ++ take 40 (TL.unpack a) ++ "...", a))
  [ "(+ 1 20)"
  , "(+ (+ 2 20) 0)"
  , "(+ (+ 3 20) (+ 10 20))"
  , "(+ (+ (+ 4 20) (+ 10 20)) 0)"
  , "(+ (+ (+ 5 20) (+ 10 20)) (+ 10 20))"
  , "(+ (+ (+ 6 20) (+ 10 20)) (+ (+ 10 20) 0))"
  , "(+ (+ (+ 7 20) (+ 10 20)) (+ (+ 10 20) (+ 10 20)))"
  , "(cond :pred (+ 42 x) :false (fibonacci :args 3) :true (factorial :args (* 10 (+ 1 2))))"
  , "(invert (* (+ (cond :pred (+ 42 314) :false (fibonacci :args 3) :true (factorial :args (* 10 (+ 1 2)))) (cond :pred (+ 42 28) :false (fibonacci :args 3) :true (factorial :args (* 10 (+ 1 2))))) (+ (cond :pred (+ 42 314) :false (fibonacci :args 3) :true (factorial :args (* 10 (+ foo bar)))) (cond :pred (+ 42 314) :false (fibonacci :args 3) :true (factorial :args (* 10 (+ 1 2)))))))"
  ]

benchCases_expr :: [(String, Expr)]
benchCases_expr = map (second expr) benchCases

benchCases_sexp :: [(String, Sexp)]
benchCases_sexp = map (second (either error id . genExpr)) benchCases_expr

main :: IO ()
main = defaultMain
  [ bgroup "generation" . map (\(name, expr) -> bench name $ whnf genExpr expr) $ benchCases_expr
  , bgroup "parsing" . map (\(name, sexp) -> bench name $ whnf parseExpr sexp) $ benchCases_sexp
  ]
