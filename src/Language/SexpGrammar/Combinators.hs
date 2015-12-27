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

import Prelude hiding ((.), id)

import Control.Category

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
bool = Inject . GAtom . Inject $ GBool

integer :: Grammar SexpGrammar (Sexp :- t) (Integer :- t)
integer = Inject . GAtom . Inject $ GInt

int :: Grammar SexpGrammar (Sexp :- t) (Int :- t)
int = ParsePrism (inStack (iso fromIntegral fromIntegral)) . integer

real :: Grammar SexpGrammar (Sexp :- t) (Scientific :- t)
real = Inject . GAtom . Inject $ GReal

double :: Grammar SexpGrammar (Sexp :- t) (Double :- t)
double = ParsePrism (inStack (iso fromFloatDigits toRealFloat)) . real

string :: Grammar SexpGrammar (Sexp :- t) (Text :- t)
string = Inject . GAtom . Inject $ GString

string' :: Grammar SexpGrammar (Sexp :- t) (String :- t)
string' = ParsePrism (inStack (iso pack unpack)) . string

symbol :: Grammar SexpGrammar (Sexp :- t) (Text :- t)
symbol = Inject . GAtom . Inject $ GSymbol

symbol' :: Grammar SexpGrammar (Sexp :- t) (String :- t)
symbol' = ParsePrism (inStack (iso pack unpack)) . symbol

keyword :: Grammar SexpGrammar (Sexp :- t) (Text :- t)
keyword = Inject . GAtom . Inject $ GKeyword

keyword' :: Grammar SexpGrammar (Sexp :- t) (String :- t)
keyword' = ParsePrism (inStack (iso pack unpack)) . keyword

sym :: Text -> Grammar SexpGrammar (Sexp :- b) b
sym = Inject . GAtom . Inject . GSym

kw :: Text -> Grammar SexpGrammar (Sexp :- b) b
kw = Inject . GAtom . Inject . GKw
