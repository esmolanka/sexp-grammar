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
  ( Sexp
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
  -- * TemplateHaskell helpers
  , grammarFor
  -- * GHC.Generics helpers
  , match
  , Coproduct (..)
  -- * Grammar types
  , SexpGrammar
  , AtomGrammar
  , SeqGrammar
  , PropGrammar
  -- * Parsing and printing
  , parseFromString
  , parseFromFile
  , prettyToText
  , prettyToFile
  -- ** Low-level printing and parsing
  , parse
  , gen
  -- * Typeclass for Sexp grammars
  , SexpIso (..)
  -- * Re-exported from stack-prism
  , StackPrism
  , (:-) (..)
  ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.StackPrism
import Data.InvertibleGrammar
import Data.InvertibleGrammar.TH
import Data.InvertibleGrammar.Generic
import Language.Sexp (Sexp)
import Language.SexpGrammar.Base
import Language.SexpGrammar.Combinators
import Language.SexpGrammar.Class
import Language.SexpGrammar.Parser

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import Language.Sexp (parseSexp, printSexp)

parseFromString :: SexpG a -> String -> Either String a
parseFromString g input =
  parseSexp "<string>" input >>= runParser (parse g)

parseFromFile :: SexpG a -> FilePath -> IO (Either String a)
parseFromFile g fn = do
  str <- readFile fn
  return $ parseSexp fn str >>= runParser (parse g)

prettyToText :: SexpG a -> a -> Either String Text
prettyToText g =
  fmap printSexp . runParser (gen g)

prettyToFile :: FilePath -> SexpG a -> a -> IO (Either String ())
prettyToFile fn g a = do
  case runParser (gen g) a of
    Left msg -> return $ Left msg
    Right s  -> Right <$> T.writeFile fn (printSexp s)
