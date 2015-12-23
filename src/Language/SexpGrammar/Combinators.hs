{-# LANGUAGE TypeOperators #-}

module Language.SexpGrammar.Combinators
  ( multiple
  , fromSexp
  , bool
  , symbol
  , keyword
  , list
  )
where

import Data.StackPrism
import Data.Text (Text)

import Data.InvertibleGrammar
import Language.SexpGrammar.Base
import Language.Sexp.Types

-- litAtom :: Atom -> Grammar SexpGrammar (Sexp :- t) t
-- litAtom = Gram . Head "literal atom" . LitAtom

fromSexp :: (FromSexp a) => Grammar ListGrammar t (a :- t)
fromSexp = Gram $ Head "fromSexp" $ sexpGrammar

bool :: Grammar ListGrammar t (Bool :- t)
bool = Gram $ Head "bool" $ sexpGrammar

-- | Try to match literal symbol.
symbol :: Text -> Grammar ListGrammar t t
symbol = Gram . Head "symbol" . Gram . LitAtom . AtomSymbol

-- | Try to match literal keyword.
keyword :: Text -> Grammar ListGrammar t t
keyword = Gram . Head "keyword" . Gram . LitAtom . AtomKeyword

list :: Grammar ListGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
list = Gram . DescendList

