{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Language.SexpGrammar.Combinators
  ( list
  , vect
  , el
  , rest
  , props
  , (.:)
  , (.:?)
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
  , sym
  , kw
  , coproduct
  , pair
  , unpair
  , swap
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NE
import Data.Scientific
import Data.StackPrism
import Data.Text (Text, pack, unpack)

import Data.InvertibleGrammar
import Data.InvertibleGrammar.TH
import Language.Sexp.Types
import Language.SexpGrammar.Base

----------------------------------------------------------------------
-- Sequence combinators

-- | Define a sequence grammar inside a list
list :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
list = Inject . GList

-- | Define a sequence grammar inside a vector
vect :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
vect = Inject . GVect

-- | Define a sequence element grammar
el :: Grammar SexpGrammar (Sexp :- a) b -> Grammar SeqGrammar a b
el = Inject . GElem

-- | Define a grammar for rest of the sequence
rest :: Grammar SexpGrammar (Sexp :- a) (b :- a) -> Grammar SeqGrammar a ([b] :- a)
rest = Inject . GRest

-- | Define a property list grammar on the rest of the sequence
props :: Grammar PropGrammar a b -> Grammar SeqGrammar a b
props = Inject . GProps

-- | Define property pair grammar
(.:) :: Kw -> Grammar SexpGrammar (Sexp :- t) (a :- t) -> Grammar PropGrammar t (a :- t)
(.:) name = Inject . GProp name

-- | Define optional property pair grammar
(.:?) :: Kw -> Grammar SexpGrammar (Sexp :- t) (a :- t) -> Grammar PropGrammar t (Maybe a :- t)
(.:?) name g = coproduct
  [ $(grammarFor 'Just) . (name .: g)
  , $(grammarFor 'Nothing)
  ]

----------------------------------------------------------------------
-- Atom combinators

-- | Define an atomic Bool grammar
bool :: SexpG Bool
bool = Inject . GAtom . Inject $ GBool

-- | Define an atomic Integer grammar
integer :: SexpG Integer
integer = Inject . GAtom . Inject $ GInt

-- | Define an atomic Int grammar
int :: SexpG Int
int = iso fromIntegral fromIntegral . integer

-- | Define an atomic real number (Scientific) grammar
real :: SexpG Scientific
real = Inject . GAtom . Inject $ GReal

-- | Define an atomic double precision floating point number (Double) grammar
double :: SexpG Double
double = iso toRealFloat fromFloatDigits . real

-- | Define an atomic string (Text) grammar
string :: SexpG Text
string = Inject . GAtom . Inject $ GString

-- | Define an atomic string ([Char]) grammar
string' :: SexpG String
string' = iso unpack pack . string

-- | Define a grammar for a symbol (Text)
symbol :: SexpG Text
symbol = Inject . GAtom . Inject $ GSymbol

-- | Define a grammar for a symbol ([Char])
symbol' :: SexpG String
symbol' = iso unpack pack . symbol

-- | Define a grammar for a keyword
keyword :: SexpG Kw
keyword = Inject . GAtom . Inject $ GKeyword

-- | Define a grammar for a constant symbol
sym :: Text -> SexpG_
sym = Inject . GAtom . Inject . GSym

-- | Define a grammar for a constant keyword
kw :: Kw -> SexpG_
kw = Inject . GAtom . Inject . GKw

----------------------------------------------------------------------
-- Special combinators

-- | Combine several alternative grammars into one grammar
coproduct :: [Grammar g a b] -> Grammar g a b
coproduct = sconcat . NE.fromList

-- | Construct pair from two top elements of stack
pair :: Grammar g (b :- a :- t) ((a, b) :- t)

-- | Deconstruct pair into two top elements of stack
unpair :: Grammar g ((a, b) :- t) (b :- a :- t)

(pair, unpair) = (Iso f g, Iso g f)
  where
    f = (\(b :- a :- t) -> (a, b) :- t)
    g = (\((a, b) :- t) -> (b :- a :- t))

-- | Swap two top elements of stack
swap :: Grammar g (b :- a :- t) (a :- b :- t)
swap = Iso (\(b :- a :- t) -> a :- b :- t)
           (\(a :- b :- t) -> b :- a :- t)
