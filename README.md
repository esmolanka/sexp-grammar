[![Build Status](https://travis-ci.org/esmolanka/sexp-grammar.svg?branch=master)](https://travis-ci.org/esmolanka/sexp-grammar)

sexp-grammar
============

It is a library of invertible parsing combinators for
S-expressions. The combinators -- primitive grammars -- not only
encode a way how to parse S-expressions into a Haskell value, but how
to serialise it back into an S-expression.

The approach used in `sexp-grammar` is inspired by the paper
[Invertible syntax descriptions: Unifying parsing and pretty printing]
(http://www.informatik.uni-marburg.de/~rendel/unparse/) and a similar
implementation of invertible grammar approach for JSON, library by
Martijn van Steenbergen called
[JsonGrammar2](https://github.com/MedeaMelana/JsonGrammar2).

Let's have a look at `sexp-gramar` at work:

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

instance SexpIso Person where
  sexpIso = with $ \person ->  -- Person is isomorphic to:
    list (                           -- a list with
      el (sym "person") >>>          -- a symbol "person",
      el string         >>>          -- a string, and
      props (                        -- a property-list with
        "address" .:  string >>>     -- a keyword :address and a string value, and
        "age"     .:? int))  >>>     -- an optional keyword :age with int value.
    person
```

We've just defined an isomorphism between S-expression representation
and Haskell data record representation of the same information.

```haskell
ghci> :set -XTypeApplications
ghci> import Language.SexpGrammar
ghci> import Data.ByteString.Lazy.Char8 (pack, unpack)
ghci> person <- either error return . decode @Person . pack =<< getLine
(person "John Doe" :address "42 Whatever str." :age 25)
ghci> person
Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25}
ghci> putStrLn (either id unpack (encode person))
(person "John Doe" :address "42 Whatever str." :age 25)
```

See more [examples](https://github.com/esmolanka/sexp-grammar/tree/master/examples)
in the repository.
