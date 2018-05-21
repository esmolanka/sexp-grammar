[![Build Status](https://travis-ci.org/esmolanka/sexp-grammar.svg?branch=master)](https://travis-ci.org/esmolanka/sexp-grammar)

sexp-grammar
============

Invertible syntax library for serializing and deserializing Haskell structures
into S-expressions. Just write a grammar once and get both parser and
pretty-printer, for free.

**WARNING: highly unstable and experimental software. Not intended for production**

The approach used in `sexp-grammar` is inspired by the paper
[Invertible syntax descriptions: Unifying parsing and pretty printing]
(http://www.informatik.uni-marburg.de/~rendel/unparse/) and a similar
implementation of invertible grammar approach for JSON, library by Martijn van
Steenbergen called [JsonGrammar2](https://github.com/MedeaMelana/JsonGrammar2).

Let's take a look at example:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import GHC.Generics
import Data.Text (Text)
import Language.SexpGrammar
import Language.SexpGrammar.Generic

data Person = Person
  { pName    :: Text
  , pAddress :: Text
  , pAge     :: Maybe Int
  } deriving (Show, Generic)

personGrammar :: Grammar Position (Sexp :- t) (Person :- t)
personGrammar = with $ \person ->  -- Person is isomorphic to:
  list (                           -- a list with
    el (sym "person") >>>          -- a symbol "person",
    el string         >>>          -- a string, and
    props (                        -- a property-list with
      "address" .:  string >>>     -- a keyword :address and a string value, and
      "age"     .:? int))  >>>     -- an optional keyword :age with int value.
  person
```

So now we can use `personGrammar` to parse S-expressions to records of type
`Person` and pretty-print records of type `Person` back to S-expressions:

```haskell
ghci> import Language.SexpGrammar
ghci> import qualified Data.ByteString.Lazy.Char8 as B8
ghci> person <- either error id . decodeWith personGrammar . B8.pack <$> getLine
(person "John Doe" :address "42 Whatever str." :age 25)
ghci> person
Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25}
ghci> either print B8.putStrLn . encodeWith personGrammar $ person
(person "John Doe" :address "42 Whatever str." :age 25)
```

See more [examples](https://github.com/esmolanka/sexp-grammar/tree/master/examples) in the repository.
