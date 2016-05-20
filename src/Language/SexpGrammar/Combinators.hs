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
  , enum
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
  , pair
  , unpair
  , swap
  , coproduct
  , coproduct'
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

-- | Define a property list grammar on the rest of the sequence. The
-- remaining sequence must be empty or start with a keyword and its
-- corresponding value and continue with the sequence built by the
-- same rules.
--
-- E.g.
--
-- > :kw1 <val1> :kw2 <val2> ... :kwN <valN>
props :: Grammar PropGrammar a b -> Grammar SeqGrammar a b
props = Inject . GProps

-- | Define property pair grammar
(.:) :: Kw -> Grammar SexpGrammar (Sexp :- t) (a :- t) -> Grammar PropGrammar t (a :- t)
(.:) name = Inject . GProp name

-- | Define optional property pair grammar
(.:?) :: Kw -> Grammar SexpGrammar (Sexp :- t) (a :- t) -> Grammar PropGrammar t (Maybe a :- t)
(.:?) name = Inject . GOptProp name

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

-- | Define a grammar for an enumeration type. Automatically derives
-- all symbol names from data constructor names and \"lispifies\" them.
enum :: (Enum a, Bounded a, Eq a, Data a) => SexpG a
enum = coproduct $ map (\a -> push a . sym (getEnumName a)) [minBound .. maxBound]
  where
    getEnumName :: (Data a) => a -> Text
    getEnumName = pack . lispifyName . showConstr . toConstr

-- | Define a grammar for a constant symbol
sym :: Text -> SexpG_
sym = Inject . GAtom . Inject . GSym

-- | Define a grammar for a constant keyword
kw :: Kw -> SexpG_
kw = Inject . GAtom . Inject . GKw

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

coproduct'
  :: forall g a b t.
     (Typeable b) =>
     [Grammar g (a :- t) (b :- t)]
  -> Grammar g (a :- t) (b :- t)
coproduct' = foldr (:<>:) catchAll
  where
    r :: Proxy b
    r = Proxy
    typeName = tyConName . typeRepTyCon . typeRep $ r
    catchAll =
      embedPrism      typeName undefined (const Nothing) >>>
      embedParsePrism typeName undefined (const Nothing)

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
