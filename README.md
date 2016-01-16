sexp-grammar
============

Invertible syntax library for serializing and deserializing Haskell
structures into S-expressions. Just write a grammar once and get
both parser and pretty-printer, for free.

The package is heavily inspired by the paper
[Invertible syntax descriptions: Unifying parsing and pretty printing]
(http://www.informatik.uni-marburg.de/~rendel/unparse/) and a similar
implementation of invertible grammar approach for JSON, library by
Martijn van Steenbergen called
[JsonGrammar2](https://github.com/MedeaMelana/JsonGrammar2).

Let's take a look at example:

```haskell
data Person = Person
  { pName    :: String
  , pAddress :: String
  , pAge     :: Maybe Int
  } deriving (Show)

personGrammar :: SexpG Person
personGrammar =
  $(grammarFor 'Person) .               -- construct Person from
    list (                              -- a list with
      el (sym "person") >>>             -- symbol "person",
      el string'        >>>             -- some string,
      props (                           -- and properties
        Kw "address" .: string' >>>     -- :address with string value,
        Kw "age" .:? int))              -- and optional :age int proprety
```

So now we can use `personGrammar` to parse S-expessions to `Person`
record and pretty-print any `Person` back to S-expression:

```haskell
ghci> :m Control.Category Language.SexpGrammar
ghci> parseFromString personGrammar <$> getLine
(person "John Doe" :address "42 Whatever str." :age 25)
Right (Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25})
ghci> let (Right person) = it
ghci> prettyToText personGrammar person
(person
 "John Doe"
 :address
 "42 Whatever str."
 :age
 25)
```

The grammars are described in terms of isomorphisms and stack
manipulations.

The simplest primitive grammars are atom grammars, which match `Sexp`
atoms with Haskell counterparts:

```haskell
                             --               grammar type   | consumes     | produces
                             --    --------------------------+--------------+-------------------
bool    :: SexpG Bool        -- or Grammar    SexpGrammar      (Sexp :- t)    (Bool       :- t)
integer :: SexpG Integer     -- or Grammar    SexpGrammar      (Sexp :- t)    (Integer    :- t)
int     :: SexpG Int         -- or Grammar    SexpGrammar      (Sexp :- t)    (Int        :- t)
real    :: SexpG Scientific  -- or Grammar    SexpGrammar      (Sexp :- t)    (Scientific :- t)
double  :: SexpG Double      -- or Grammar    SexpGrammar      (Sexp :- t)    (Double     :- t)
string  :: SexpG Text        -- or Grammar    SexpGrammar      (Sexp :- t)    (Text       :- t)
string' :: SexpG String      -- or Grammar    SexpGrammar      (Sexp :- t)    (String     :- t)
symbol  :: SexpG Text        -- or Grammar    SexpGrammar      (Sexp :- t)    (Text       :- t)
symbol' :: SexpG String      -- or Grammar    SexpGrammar      (Sexp :- t)    (String     :- t)
keyword :: SexpG Kw          -- or Grammar    SexpGrammar      (Sexp :- t)    (Kw         :- t)
sym     :: Text -> SexpG_    -- or Grammar    SexpGrammar      (Sexp :- t)    t
kw      :: Kw   -> SexpG_    -- or Grammar    SexpGrammar      (Sexp :- t)    t
```

Grammars matching lists and vectors can be defined using an auxiliary
grammar type `SeqGrammar`. The following primitives embed
`SeqGrammar`s into main `SexpGrammar` context:

```haskell
list  :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
vect  :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
```

Grammar type `SeqGrammar` basically describes the sequence of elements
in a `Sexp` list (or vector). Single element grammar is defined with
`el`, "match rest of the sequence as list" grammar could be defined
with `rest` combinator. If the rest of the sequence is a property
list, `props` combinator should be used.

```haskell
el    :: Grammar SexpGrammar (Sexp :- a)  b       -> Grammar SeqGrammar a b
rest  :: Grammar SexpGrammar (Sexp :- a) (b :- a) -> Grammar SeqGrammar a ([b] :- a)
props :: Grammar PropGrammar a b                  -> Grammar SeqGrammar a b
```

`props` combinator embeds properties grammar `PropGrammar` into a
`SeqGrammar` context. `PropGrammar` describes what keys and values to
match.

```haskell
(.:)  :: Kw
      -> Grammar SexpGrammar (Sexp :- t) (a :- t)
      -> Grammar PropGrammar t (a :- t)

(.:?) :: Kw
      -> Grammar SexpGrammar (Sexp :- t) (a :- t)
      -> Grammar PropGrammar t (Maybe a :- t)
```

Please refer to Haddock on [Hackage](http://hackage.haskell.org/package/sexp-grammar)
for API documentation.

Diagram of grammar contexts:

```

     --------------------------------------
     |              AtomGrammar           |
     --------------------------------------
         ^
         |  atomic grammar combinators
         v
 ------------------------------------------------------
 |                      SexpGrammar                   |
 ------------------------------------------------------
         | list, vect     ^              ^
         v                | el, rest     |
     ----------------------------------  |
     |           SeqGrammar           |  |
     ----------------------------------  | (.:)
              | props                    | (.:?)
              v                          |
          -------------------------------------
          |             PropGrammar           |
          -------------------------------------

```
