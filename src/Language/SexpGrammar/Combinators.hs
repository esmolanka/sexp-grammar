{-# LANGUAGE TypeOperators #-}

module Language.SexpGrammar.Combinators
  ( multiple
  , list
  , el
  , bool
  , symbol
  , keyword
  )
where

import Data.StackPrism
import Data.Text (Text)

import Data.InvertibleGrammar
import Language.SexpGrammar.Base
import Language.SexpGrammar.Class
import Language.Sexp.Types


list :: Grammar ListGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
list = Inject . GList

el :: Grammar SexpGrammar (Sexp :- a) b -> Grammar ListGrammar a b
el = Inject . GElem "fromSexp"

bool :: Grammar ListGrammar t (Bool :- t)
bool = el sexpGrammar

symbol :: Text -> Grammar SexpGrammar (Sexp :- b) b
symbol = Inject . GAtom . AtomSymbol

keyword :: Text -> Grammar SexpGrammar (Sexp :- b) b
keyword = Inject . GAtom . AtomKeyword

