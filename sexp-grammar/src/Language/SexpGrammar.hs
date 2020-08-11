{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE Safe          #-}
{-# LANGUAGE TypeOperators #-}

{- |

Write your grammar once and get both parser and pretty-printer, for
free.

> import GHC.Generics
> import Data.Text (Text)
> import Language.SexpGrammar
> import Language.SexpGrammar.Generic
>
> data Person = Person
>   { pName    :: Text
>   , pAddress :: Text
>   , pAge     :: Maybe Int
>   } deriving (Show, Generic)
>
> instance SexpIso Person where
>   sexpIso = with $ \person ->  -- Person is isomorphic to:
>     list (                           -- a list with
>       el (sym "person") >>>          -- a symbol "person",
>       el string         >>>          -- a string, and
>       props (                        -- a property-list with
>         "address" .:  string >>>     -- a keyword :address and a string value, and
>         "age"     .:? int))  >>>     -- an optional keyword :age with int value.
>     person

So now we can use this isomorphism to parse S-expessions to @Person@
record and pretty-print @Person@ records back to S-expression.

> (person "John Doe" :address "42 Whatever str." :age 25)

will parse into:

> Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25}

and the record will pretty-print back into:

> (person "John Doe" :address "42 Whatever str." :age 25)

-}

module Language.SexpGrammar
  ( -- * Data types
    Sexp
  , Position
  , SexpGrammar
  , Grammar
  , (:-)
  , SexpIso (..)
  -- * Encoding
  , toSexp
  , encode
  , encodeWith
  , encodePretty
  , encodePrettyWith
  -- * Decoding
  , fromSexp
  , decode
  , decodeWith
  -- * Combinators
  , module Control.Category
  , module Data.InvertibleGrammar.Combinators
  , module Language.SexpGrammar.Base
  -- * Error reporting
  , Mismatch
  , expected
  , unexpected
  ) where

import Control.Category ((<<<), (>>>))

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Combinators

import Language.Sexp.Located (Sexp, Position)
import qualified Language.Sexp.Located as Sexp

import Language.SexpGrammar.Base
import Language.SexpGrammar.Class

----------------------------------------------------------------------
-- Sexp interface

-- | Run grammar in parsing (left-to-right) direction
--
-- > fromSexp g = runGrammarString Sexp.dummyPos . forward (sealed g)
fromSexp :: SexpGrammar a -> Sexp -> Either String a
fromSexp g =
  runGrammarString Sexp.dummyPos .
    forward (sealed g)

-- | Run grammar in generating (right-to-left) direction
--
-- > toSexp g = runGrammarString Sexp.dummyPos . backward (sealed g)
toSexp :: SexpGrammar a -> a -> Either String Sexp
toSexp g =
  runGrammarString Sexp.dummyPos .
    backward (sealed g)

----------------------------------------------------------------------

-- | Serialize a value using 'SexpIso' instance
encode :: SexpIso a => a -> Either String ByteString
encode =
  encodeWith sexpIso

-- | Serialise a value using a provided grammar
encodeWith :: SexpGrammar a -> a -> Either String ByteString
encodeWith g =
  fmap Sexp.encode . toSexp g

-- | Serialise and pretty-print a value using its 'SexpIso' instance
encodePretty :: SexpIso a => a -> Either String ByteString
encodePretty =
  encodePrettyWith sexpIso

-- | Serialise and pretty-print a value using a provided grammar
encodePrettyWith :: SexpGrammar a -> a -> Either String ByteString
encodePrettyWith g =
  fmap Sexp.format . toSexp g

----------------------------------------------------------------------

-- | Deserialise a value using its 'SexpIso' instance
decode :: SexpIso a => ByteString -> Either String a
decode =
  decodeWith sexpIso "<string>"

-- | Deserialise a value using provided grammar, use a provided file
-- name for error messages
decodeWith :: SexpGrammar a -> FilePath -> ByteString -> Either String a
decodeWith g fn input =
  Sexp.parseSexp fn input >>= fromSexp g
