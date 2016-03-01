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
  -- * Combinators
  -- ** Primitive grammars
  , iso
  , embedPrism
  , embedParsePrism
  , push
  , pushForget
  , module Language.SexpGrammar.Combinators
  -- * Grammar types
  , SexpGrammar
  , AtomGrammar
  , SeqGrammar
  , PropGrammar
  -- * Decoding and encoding using SexpIso typeclass
  , decode
  , encode
  , encodePretty
  , decode'
  , encode'
  , encodePretty'
  -- * Parsing and encoding to Sexp
  , parseSexp
  , genSexp
  -- * Typeclass for Sexp grammars
  , SexpIso (..)
  -- * Re-exported from stack-prism
  , StackPrism
  , (:-) (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.InvertibleGrammar
import Data.StackPrism

import Language.Sexp (Sexp)
import qualified Language.Sexp as Sexp

import Language.SexpGrammar.Base
import Language.SexpGrammar.Class
import Language.SexpGrammar.Combinators
import Language.SexpGrammar.Parser (runR)

----------------------------------------------------------------------
-- Sexp interface

parseSexp :: SexpG a -> Sexp -> Either String a
parseSexp g = runR (runParse g)

genSexp :: SexpG a -> a -> Either String Sexp
genSexp g = runR (runGen g)

----------------------------------------------------------------------
-- ByteString interface

decode :: SexpIso a => ByteString -> Either String a
decode input =
  Sexp.decode input >>= parseSexp sexpIso

encode :: SexpIso a => a -> Either String ByteString
encode =
  fmap Sexp.encode . genSexp sexpIso

encodePretty :: SexpIso a => a -> Either String ByteString
encodePretty =
  fmap Sexp.encodePretty . genSexp sexpIso

decode' :: SexpG a -> ByteString -> Either String a
decode' g input =
  Sexp.decode input >>= parseSexp g

encode' :: SexpG a -> a -> Either String ByteString
encode' g =
  fmap Sexp.encode . genSexp g

encodePretty' :: SexpG a -> a -> Either String ByteString
encodePretty' g =
  fmap Sexp.encodePretty . genSexp g
