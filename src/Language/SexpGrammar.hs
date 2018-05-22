{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{- |

Write your grammar once and get both parser and pretty-printer, for
free.

> data Person = Person
>   { pName    :: Text
>   , pAddress :: Text
>   , pAge     :: Maybe Int
>   } deriving (Show, Generic)
>
> personGrammar :: Grammar Position (Sexp :- t) (Person :- t)
> personGrammar = with $ \person ->  -- Person is isomorphic to:
>   list (                           -- a list with
>     el (sym "person") >>>          -- a symbol "person",
>     el string         >>>          -- a string, and
>     props (                        -- a property-list with
>       "address" .:  string >>>     -- a keyword :address and a string value, and
>       "age"     .:? int))  >>>     -- an optional keyword :age with int value.
>   person

So now we can use @personGrammar@ to parse S-expessions to @Person@
record and pretty-print any @Person@ back to S-expression.

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
import Data.InvertibleGrammar.Base
import Data.InvertibleGrammar.Combinators

import Language.Sexp (Sexp, Position)
import qualified Language.Sexp as Sexp

import Language.SexpGrammar.Base
import Language.SexpGrammar.Class

type SexpGrammar a = forall t. Grammar Position (Sexp :- t) (a :- t)

----------------------------------------------------------------------
-- Sexp interface

runParse :: Grammar Position (Sexp :- ()) (a :- ()) -> Sexp -> ContextError (Propagation Position) (GrammarError Position) a
runParse gram input =
  (\(x :- _) -> x) <$> forward gram (input :- ())

runGen :: Grammar Position (Sexp :- ()) (a :- ()) -> a -> ContextError (Propagation Position) (GrammarError Position) Sexp
runGen gram input =
  (\(x :- _) -> x) <$> backward gram (input :- ())


-- | Run grammar in parsing direction
fromSexp :: SexpGrammar a -> Sexp -> Either String a
fromSexp g a =
  runGrammar Sexp.dummyPos showPos (runParse g a)
  where
    showPos (Sexp.Position fn line col) = fn ++ ":" ++ show line ++ ":" ++ show col

-- | Run grammar in generating direction
toSexp :: SexpGrammar a -> a -> Either String Sexp
toSexp g a =
  runGrammar Sexp.dummyPos (const "<no location information>") (runGen g a)

----------------------------------------------------------------------

-- | Serialize a value using @SexpIso@ instance
encode :: SexpIso a => a -> Either String ByteString
encode =
  encodeWith sexpIso

-- | Serialise a value using provided grammar
encodeWith :: SexpGrammar a -> a -> Either String ByteString
encodeWith g =
  fmap (Sexp.encode . Sexp.stripLocation) . toSexp g

-- | Serialise and pretty-print a value using @SexpIso@ instance
encodePretty :: SexpIso a => a -> Either String ByteString
encodePretty =
  encodePrettyWith sexpIso

-- | Serialise and pretty-print a value using provided grammar
encodePrettyWith :: SexpGrammar a -> a -> Either String ByteString
encodePrettyWith g =
  fmap Sexp.prettySexp . toSexp g

----------------------------------------------------------------------

-- | Deserialise a value using @SexpIso@ instance
decode :: SexpIso a => ByteString -> Either String a
decode =
  decodeWith sexpIso "<string>"

-- | Deserialise a value using provided grammar, use provided file name for error messages
decodeWith :: SexpGrammar a -> FilePath -> ByteString -> Either String a
decodeWith g fn input =
  Sexp.parseSexp fn input >>= fromSexp g
