{-# LANGUAGE TypeOperators #-}

module Language.SexpGrammar.Combinators
  ( multiple
  , list
  , el
  , bool
  , integer
  , int
  , real
  , double
  , string
  , symbol
  , keyword
  , string'
  , symbol'
  , keyword'
  , sym
  , kw
  )
where

import Data.StackPrism.Extra
import Data.Text (Text, pack, unpack)
import Data.Scientific

import Data.InvertibleGrammar
import Language.SexpGrammar.Base
import Language.Sexp.Types

list :: Grammar ListGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
list = Inject . GList

el :: Grammar SexpGrammar (Sexp :- a) b -> Grammar ListGrammar a b
el = Inject . GElem "fromSexp"

bool :: Grammar SexpGrammar (Sexp :- t) (Bool :- t)
bool = ParsePrism boolSexpP

integer :: Grammar SexpGrammar (Sexp :- t) (Integer :- t)
integer = ParsePrism $ intSexpP

int :: Grammar SexpGrammar (Sexp :- t) (Int :- t)
int = ParsePrism $ intSexpP . inStack (iso fromIntegral fromIntegral)

real :: Grammar SexpGrammar (Sexp :- t) (Scientific :- t)
real = ParsePrism realSexpP

double :: Grammar SexpGrammar (Sexp :- t) (Double :- t)
double = ParsePrism $ realSexpP . inStack (iso fromFloatDigits toRealFloat)

string :: Grammar SexpGrammar (Sexp :- t) (Text :- t)
string = ParsePrism stringSexpP

string' :: Grammar SexpGrammar (Sexp :- t) (String :- t)
string' = ParsePrism $ stringSexpP . inStack (iso pack unpack)

symbol :: Grammar SexpGrammar (Sexp :- t) (Text :- t)
symbol = ParsePrism symbolSexpP

symbol' :: Grammar SexpGrammar (Sexp :- t) (String :- t)
symbol' = ParsePrism $ symbolSexpP . inStack (iso pack unpack)

keyword :: Grammar SexpGrammar (Sexp :- t) (Text :- t)
keyword = ParsePrism keywordSexpP

keyword' :: Grammar SexpGrammar (Sexp :- t) (String :- t)
keyword' = ParsePrism $ keywordSexpP . inStack (iso pack unpack)

sym :: Text -> Grammar SexpGrammar (Sexp :- b) b
sym = Inject . GAtom . AtomSymbol

kw :: Text -> Grammar SexpGrammar (Sexp :- b) b
kw = Inject . GAtom . AtomKeyword

