{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Criterion.Main

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Control.DeepSeq
import Control.Exception

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.SexpGrammar
import qualified Language.SexpGrammar.TH as TH
import qualified Language.SexpGrammar.Generic as G
import Language.SexpGrammar.Generic (Coproduct(..))

newtype Ident = Ident Text
  deriving (Show, Eq, Generic)

data Expr
  = Var Ident
  | Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Inv Expr
  | IfZero Expr Expr (Maybe Expr)
  | Apply [Expr] String Prim -- inconvenient ordering: arguments, useless annotation, identifier
    deriving (Show, Eq, Generic)

data Prim
  = SquareRoot
  | Factorial
  | Fibonacci
    deriving (Show, Eq, Generic)

instance NFData Ident
instance NFData Prim
instance NFData Expr

return []

type SexpG a = forall t. Grammar Position (Sexp :- t) (a :- t)

instance SexpIso Prim where
  sexpIso = G.match
    $ With (sym "square-root" >>>)
    $ With (sym "factorial" >>>)
    $ With (sym "fibonacci" >>>)
    $ End

instance SexpIso Ident where
  sexpIso = $(TH.match ''Ident)
    (\_Ident -> _Ident . symbol)

exprGrammarTH :: SexpG Expr
exprGrammarTH = go
  where
    go :: SexpG Expr
    go = $(TH.match ''Expr)
      (\_Var -> _Var . sexpIso)
      (\_Lit -> _Lit . int)
      (\_Add -> _Add . list (el (sym "+") >>> el go >>> el go))
      (\_Mul -> _Mul . list (el (sym "*") >>> el go >>> el go))
      (\_Inv -> _Inv . list (el (sym "invert") >>> el go))
      (\_IfZero -> _IfZero . list (el (sym "cond") >>> props ( "pred"  .:  go
                                                           >>> "true"  .:  go
                                                           >>> "false" .:? go )))
      (\_Apply -> _Apply .                       -- Convert prim :- "dummy" :- args :- () to Apply node
          list
           (el (sexpIso :: SexpG Prim) >>>       -- Push prim:       prim :- ()
            el (sym ":args") >>>                 -- Recognize :args, push nothing
            rest (go :: SexpG Expr) >>>          -- Push args:       args :- prim :- ()
            onTail (
               swap >>>                          -- Swap:            prim :- args :- ()
               push "dummy"                      -- Push "dummy":    "dummy" :- prim :- args :- ()
                 (const True)
                 (const (expected "dummy")) >>>
               swap)                             -- Swap:            prim :- "dummy" :- args :- ()
           ))

exprGrammarGeneric :: SexpG Expr
exprGrammarGeneric = go
  where
    go :: SexpG Expr
    go = G.match
      $ With (\_Var -> _Var . sexpIso)
      $ With (\_Lit -> _Lit . int)
      $ With (\_Add -> _Add . list (el (sym "+") >>> el go >>> el go))
      $ With (\_Mul -> _Mul . list (el (sym "*") >>> el go >>> el go))
      $ With (\_Inv -> _Inv . list (el (sym "invert") >>> el go))
      $ With (\_IfZero -> _IfZero . list (el (sym "cond") >>> props ( "pred"  .:  go
                                                                  >>> "true"  .:  go
                                                                  >>> "false" .:? go )))
      $ With (\_Apply -> _Apply .                      -- Convert prim :- "dummy" :- args :- () to Apply node
                list
                 (el (sexpIso :: SexpG Prim) >>>       -- Push prim:       prim :- ()
                  el (sym ":args") >>>                 -- Recognize :args, push nothing
                  rest (go :: SexpG Expr) >>>          -- Push args:       args :- prim :- ()
                  onTail (
                     swap >>>                          -- Swap:            prim :- args :- ()
                     push "dummy"                      -- Push "dummy":    "dummy" :- prim :- args :- ()
                       (const True)
                       (const (expected "dummy")) >>>
                     swap)                             -- Swap:            prim :- "dummy" :- args :- ()
                 ))
      $ End


expr :: ByteString -> Expr
expr = either error id . decodeWith exprGrammarTH "<string>"

benchCases :: [(String, ByteString)]
benchCases = map (\a -> ("expression, size " ++ show (B8.length a) ++ " bytes", a))
  [ "(+ 1 20)"
  , "(cond :pred (+ 42 x) :false (fibonacci :args 3) :true (factorial :args (* 10 (+ 1 2))))"
  , "(invert (* (+ (cond :pred (+ 42 314) :false (fibonacci :args 3) :true (factorial :args \
    \(* 10 (+ 1 2)))) (cond :pred (+ 42 28) :false (fibonacci :args 3) :true (factorial :args \
    \(* 10 (+ 1 2))))) (+ (cond :pred (+ 42 314) :false (fibonacci :args 3) :true (factorial \
    \:args (* 10 (+ foo bar)))) (cond :pred (+ 42 314) :false (fibonacci :args 3) :true (factorial \
    \:args (* 10 (+ 1 2)))))))"
  ]

mkBenchmark :: String -> ByteString -> IO Benchmark
mkBenchmark name str = do
  expr <- evaluate $ force $ expr str
  sexp <- evaluate $ force $ either error id (toSexp exprGrammarTH expr)
  return $ bgroup name
    [ bench "gen"    $ nf (toSexp exprGrammarTH) expr
    , bench "genG"   $ nf (toSexp exprGrammarGeneric) expr
    , bench "parse"  $ nf (fromSexp exprGrammarTH) sexp
    , bench "parseG" $ nf (fromSexp exprGrammarGeneric) sexp
    ]

main :: IO ()
main = do
  cases <- mapM (uncurry mkBenchmark) benchCases
  defaultMain cases
