{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

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
>         Kw "age"     .:? int ))         -- and optional :age int proprety

So now we can use @personGrammar@ to parse S-expessions to @Person@
record and pretty-print any @Person@ back to S-expression.

> (person "John Doe" :address "42 Whatever str." :age 25)

will parse into:

> Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25}

and the record will pretty-print back into:

> (person
>  "John Doe"
>  :address
>  "42 Whatever str."
>  :age
>  25)

Grammar types diagram:

>     --------------------------------------
>     |              AtomGrammar           |
>     --------------------------------------
>         ^
>         |  atomic grammar combinators
>         v
> ------------------------------------------------------
> |                      SexpGrammar                   |
> ------------------------------------------------------
>         | list, vect     ^              ^
>         v                | el, rest     |
>     ----------------------------------  |
>     |           SeqGrammar           |  |
>     ----------------------------------  | (.:)
>              | props                    | (.:?)
>              v                          |
>          -------------------------------------
>          |             PropGrammar           |
>          -------------------------------------

-}

module Language.SexpGrammar
  ( Sexp (..)
  , Sexp.Atom (..)
  , Sexp.Kw (..)
  , Grammar
  , SexpG
  , SexpG_
  , (:-) (..)
  -- * Combinators
  -- ** Primitive grammars
  , iso
  , osi
  , partialIso
  , partialOsi
  , push
  , pushForget
  , module Language.SexpGrammar.Combinators
  -- * Grammar types
  , SexpGrammar
  , AtomGrammar
  , SeqGrammar
  , PropGrammar
  -- * Decoding and encoding (machine-oriented)
  , decode
  , decodeWith
  , encode
  , encodeWith
  -- * Parsing and printing (human-oriented)
  , parse
  , parseWith
  , encodePretty
  , encodePrettyWith
  -- * Parsing and encoding to Sexp
  , parseSexp
  , genSexp
  -- * Typeclass for Sexp grammars
  , SexpIso (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Monad

import Language.Sexp (Sexp)
import qualified Language.Sexp as Sexp

import Language.SexpGrammar.Base
import Language.SexpGrammar.Class
import Language.SexpGrammar.Combinators

----------------------------------------------------------------------
-- Sexp interface

-- | Run grammar in parsing direction
parseSexp :: SexpG a -> Sexp -> Either String a
parseSexp g a = runGrammarMonad (runParse g a)
{-# INLINE parseSexp #-}

-- | Run grammar in generating direction
genSexp :: SexpG a -> a -> Either String Sexp
genSexp g a = runGrammarMonad (runGen g a)
{-# INLINE genSexp #-}

----------------------------------------------------------------------
-- ByteString interface (machine-oriented)

-- | Deserialize a value from a lazy 'ByteString'. The input must
-- contain exactly one S-expression. Comments are ignored.
decode :: SexpIso a => ByteString -> Either String a
decode =
  decodeWith sexpIso
{-# INLINE decode #-}

-- | Like 'decode' but uses specified grammar.
decodeWith :: SexpG a -> ByteString -> Either String a
decodeWith g input =
  Sexp.decode input >>= parseSexp g
{-# INLINE decodeWith #-}


-- | Serialize a value as a lazy 'ByteString' with a non-formatted S-expression
encode :: SexpIso a => a -> Either String ByteString
encode =
  encodeWith sexpIso
{-# INLINE encode #-}

-- | Like 'encode' but uses specified grammar.
encodeWith :: SexpG a -> a -> Either String ByteString
encodeWith g =
  fmap Sexp.encode . genSexp g
{-# INLINE encodeWith #-}

----------------------------------------------------------------------
-- ByteString interface (human-oriented)

-- | Parse a value from 'ByteString'. The input must contain exactly
-- one S-expression. Unlike 'decode' it takes an additional argument
-- with a file name which is being parsed. It is used for error
-- messages.
parse :: SexpIso a => FilePath -> ByteString -> Either String a
parse fn =
  parseWith sexpIso fn
{-# INLINE parse #-}

-- | Like 'parse' but uses specified grammar.
parseWith :: SexpG a -> FilePath -> ByteString -> Either String a
parseWith g fn input =
  Sexp.parseSexp fn input >>= parseSexp g
{-# INLINE parseWith #-}

-- | Pretty-prints a value serialized to a lazy 'ByteString'.
encodePretty :: SexpIso a => a -> Either String ByteString
encodePretty =
  encodePrettyWith sexpIso
{-# INLINE encodePretty #-}

-- | Like 'encodePretty' but uses specified grammar.
encodePrettyWith :: SexpG a -> a -> Either String ByteString
encodePrettyWith g =
  fmap Sexp.prettySexp . genSexp g
{-# INLINE encodePrettyWith #-}
