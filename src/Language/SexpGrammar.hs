{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{- |

Write your grammar once and get both parser and pretty-printer, for
free.

> data Person = Person
>   { pName    :: String
>   , pAddress :: String
>   , pAge     :: Maybe Int
>   } deriving (Show)
>
> personGrammar :: SexpG Person
> personGrammar =
>   $(grammarFor 'Person) .               -- construct Person from
>     list (                              -- a list with
>       el (sym "person") >>>             -- symbol "person",
>       el string'        >>>             -- some string,
>       props (                           -- and properties
>         Kw "address" .:  string' >>>    -- :address with string value,
>         Kw "age"     .:? int ))         -- and optional :age int property

So now we can use @personGrammar@ to parse S-expessions to @Person@
record and pretty-print any @Person@ back to S-expression.

> (person "John Doe" :address "42 Whatever str." :age 25)

will parse into:

> Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25}

and the record will pretty-print back into:

> (person "John Doe" :address "42 Whatever str." :age 25)

-}

module Language.SexpGrammar
  ( Sexp
  , SexpGrammar
  , Grammar (..)
  , (:-) (..)
  , SexpIso (..)
  -- * Combinators
  , module Data.InvertibleGrammar.Combinators
  , module Language.SexpGrammar.Base
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
  , decodeNamed
  , decodeNamedWith
  -- * Parsing and encoding to Sexp
  , Mismatch
  , expected
  , unexpected
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text.Lazy as TL
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
encodePretty :: SexpIso a => a -> Either String TL.Text
encodePretty =
  encodePrettyWith sexpIso

-- | Serialise and pretty-print a value using provided grammar
encodePrettyWith :: SexpGrammar a -> a -> Either String TL.Text
encodePrettyWith g =
  fmap Sexp.prettySexp . toSexp g

----------------------------------------------------------------------

-- | Deserialise a value using @SexpIso@ instance
decode :: SexpIso a => TL.Text -> Either String a
decode =
  decodeWith sexpIso

-- | Deserialise a value using provided grammar
decodeWith :: SexpGrammar a -> TL.Text -> Either String a
decodeWith g input =
  Sexp.parseSexp "<string>" input >>= fromSexp g

-- | Deserialise a value using @SexpIso@ instance, include a file name to error-messages
decodeNamed :: SexpIso a => FilePath -> TL.Text -> Either String a
decodeNamed fn =
  decodeNamedWith sexpIso fn

-- | Deserialise a value using provided grammar, include a file name to error-messages
decodeNamedWith :: SexpGrammar a -> FilePath -> TL.Text -> Either String a
decodeNamedWith g fn input =
  Sexp.parseSexp fn input >>= fromSexp g
