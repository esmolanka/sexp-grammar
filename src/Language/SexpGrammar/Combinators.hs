{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Language.SexpGrammar.Combinators
  (
  -- ** Atom grammars
    bool
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
  -- ** Complex grammars
  , list
  , vect
  -- *** Sequence grammars
  , el
  , rest
  , props
  -- *** Property grammars
  , (.:)
  , (.:?)
  -- ** Utility grammars
  , position
  , pair
  , unpair
  , swap
  , coproduct
  , enum
  , toDefault
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Data.Data
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NE
import Data.Scientific
import Data.Text (Text, pack, unpack)

import Data.InvertibleGrammar
import Language.Sexp.Types
import Language.Sexp.Utils (lispifyName)
import Language.SexpGrammar.Base

----------------------------------------------------------------------
-- Sequence combinators

-- | Sequence grammar inside a list
list :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
list = Inject . GList

-- | Sequence grammar in a vector
vect :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
vect = Inject . GVect

-- | Grammar of one element in a sequence
el :: Grammar SexpGrammar (Sexp :- a) b -> Grammar SeqGrammar a b
el = Inject . GElem

-- | Grammar of all the rest elements in a sequence
rest :: Grammar SexpGrammar (Sexp :- a) (b :- a) -> Grammar SeqGrammar a ([b] :- a)
rest = Inject . GRest

-- | Property list grammar in a sequence.
--
-- E.g.
--
-- > :kw1 <val1> :kw2 <val2> ... :kwN <valN>
props :: Grammar PropGrammar a b -> Grammar SeqGrammar a b
props = Inject . GProps

-- | Key\/value pair grammar
(.:) :: Kw -> Grammar SexpGrammar (Sexp :- t) (a :- t) -> Grammar PropGrammar t (a :- t)
(.:) name = Inject . GProp name

-- | Optional key\/value pair grammar
(.:?) :: Kw -> Grammar SexpGrammar (Sexp :- t) (a :- t) -> Grammar PropGrammar t (Maybe a :- t)
(.:?) name = Inject . GOptProp name

----------------------------------------------------------------------
-- Atom combinators

-- | Boolean grammar
bool :: SexpG Bool
bool = Inject . GAtom . Inject $ GBool

-- | Integer number grammar
integer :: SexpG Integer
integer = Inject . GAtom . Inject $ GInt

-- | Integer number grammar
int :: SexpG Int
int = iso fromIntegral fromIntegral . integer

-- | Real number grammar
real :: SexpG Scientific
real = Inject . GAtom . Inject $ GReal

-- | Double precision floating point number grammar
double :: SexpG Double
double = iso toRealFloat fromFloatDigits . real

-- | String grammar
string :: SexpG Text
string = Inject . GAtom . Inject $ GString

-- | String grammar
string' :: SexpG String
string' = iso unpack pack . string

-- | Symbol grammar
symbol :: SexpG Text
symbol = Inject . GAtom . Inject $ GSymbol

-- | Symbol grammar
symbol' :: SexpG String
symbol' = iso unpack pack . symbol

-- | Keyword grammar
keyword :: SexpG Kw
keyword = Inject . GAtom . Inject $ GKeyword

-- | Constant symbol grammar
sym :: Text -> SexpG_
sym = Inject . GAtom . Inject . GSym

-- | Constant keyword grammar
kw :: Kw -> SexpG_
kw = Inject . GAtom . Inject . GKw

-- | Get position of Sexp. Doesn't consume Sexp and doesn't have any
-- effect on backward run.
position :: Grammar SexpGrammar (Sexp :- t) (Position :- Sexp :- t)
position = Inject GPos


----------------------------------------------------------------------
-- Special combinators

-- | Combine several alternative grammars into one grammar. Useful for
-- defining grammars for sum types.
--
-- E.g. consider a data type:
--
-- > data Maybe a = Nothing | Just a
--
-- A total grammar which would handle both cases should be constructed
-- with 'coproduct' combinator or with @Semigroup@'s instance.
--
-- > maybeGrammar :: SexpG a -> SexpG (Maybe a)
-- > maybeGrammar g =
-- >   coproduct
-- >     [ $(grammarFor 'Nothing) . kw (Kw "nil")
-- >     , $(grammarFor 'Just)    . g
-- >     ]
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

-- | Swap two top elements of stack. Useful for defining grammars for
-- data constructors with inconvenient field order.
--
-- E.g. consider a data type, which has field order different from
-- what would like to display to user:
--
-- > data Command = Command { args :: [String], executable :: FilePath }
--
-- In S-expression executable should go first:
--
-- > commandGrammar =
-- >   $(grammarFor 'Command) .
-- >     list ( el (sym "call") >>>  -- symbol "call"
-- >            el string'      >>>  -- executable name
-- >            rest string'    >>>  -- arguments
-- >            swap )
swap :: Grammar g (b :- a :- t) (a :- b :- t)
swap = Iso (\(b :- a :- t) -> a :- b :- t)
           (\(a :- b :- t) -> b :- a :- t)


----------------------------------------------------------------------

-- | Enumeration grammar. Automatically derives all symbol names from
-- data constructor names and \"lispifies\" them.
enum :: (Enum a, Bounded a, Eq a, Data a) => SexpG a
enum = coproduct $ map (\a -> push a . sym (getEnumName a)) [minBound .. maxBound]
  where
    getEnumName :: (Data a) => a -> Text
    getEnumName = pack . lispifyName . showConstr . toConstr

-- | Grammar of types with \"default\" values.
toDefault :: (Eq a) => a -> Grammar g (Maybe a :- t) (a :- t)
toDefault def = iso
  (maybe def id)
  (\val -> if val == def then Nothing else Just val)
