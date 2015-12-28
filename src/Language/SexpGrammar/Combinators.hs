{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes    #-}

module Language.SexpGrammar.Combinators
  ( list
  , vect
  , el
  , rest
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
  , fx
  , pair
  )
where

import Prelude hiding ((.), id)

import Control.Category
import Data.Functor.Foldable (Fix (..))
import Data.Scientific
import Data.StackPrism
import Data.Text (Text, pack, unpack)
import Data.Coerce

import Data.InvertibleGrammar
import Language.Sexp.Types
import Language.SexpGrammar.Base

----------------------------------------------------------------------
-- List combinators

list :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
list = Inject . GList

vect :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
vect = Inject . GVect

el :: Grammar SexpGrammar (Sexp :- a) b -> Grammar SeqGrammar a b
el = Inject . GElem

rest :: Grammar SexpGrammar (Sexp :- a) (b :- a) -> Grammar SeqGrammar a ([b] :- a)
rest = Inject . GRest

----------------------------------------------------------------------
-- Atom combinators

bool :: SexpG Bool
bool = Inject . GAtom . Inject $ GBool

integer :: SexpG Integer
integer = Inject . GAtom . Inject $ GInt

int :: SexpG Int
int = iso fromIntegral fromIntegral . integer

real :: SexpG Scientific
real = Inject . GAtom . Inject $ GReal

double :: SexpG Double
double = iso toRealFloat fromFloatDigits . real

string :: SexpG Text
string = Inject . GAtom . Inject $ GString

string' :: SexpG String
string' = iso unpack pack . string

symbol :: SexpG Text
symbol = Inject . GAtom . Inject $ GSymbol

symbol' :: SexpG String
symbol' = iso unpack pack . symbol

keyword :: SexpG Text
keyword = Inject . GAtom . Inject $ GKeyword

keyword' :: SexpG String
keyword' = iso unpack pack . keyword

sym :: Text -> SexpG_
sym = Inject . GAtom . Inject . GSym

kw :: Text -> SexpG_
kw = Inject . GAtom . Inject . GKw

----------------------------------------------------------------------
-- Special combinators

fx :: Grammar g (f (Fix f) :- t) (Fix f :- t)
fx = iso coerce coerce

pair :: Grammar g (b :- a :- t) ((a, b) :- t)
pair = Iso (\(b :- a :- t) -> (a, b) :- t) (\((a, b) :- t) -> (b :- a :- t))
