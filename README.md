sexp-grammar
============

Invertible syntax library for serializing and deserializing Haskell
structures into S-expressions. Just write a grammar once and get
both parser and pretty-printer, for free.

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
      el string' >>>                    -- some string,
      props (                           -- and properties
        Kw "address" .: string' >>>     -- :address with string value,
        Kw "age" .:? int))              -- and optional :age int proprety
```

So now we can use `personGrammar` to parse S-expessions to `Person`
record and pretty-print any `Person` back to S-expression.

```
(person "John Doe" :address "42 Whatever str." :age 25)
```

will parse into:

```haskell
Person {pName = "John Doe", pAddress = "42 Whatever str.", pAge = Just 25}
```

and the record will pretty-print back into:

```
(person
 "John Doe"
 :address
 "42 Whatever str."
 :age
 25)
```

The grammars are described in terms of isomorphisms and stack
manipulations.

The simplest grammars are atom grammars, which match Sexp atoms with
Haskell counterparts:

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

Grammars for lists and vectors can be defined using an auxiliary
grammar `SeqGrammar`:

```haskell
list  :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
vect  :: Grammar SeqGrammar t t' -> Grammar SexpGrammar (Sexp :- t) t'
```

`SeqGrammar` basically describes the sequence of elements in a Sexp
list (or vector). Single element grammar is defined with `el`, "rest
of the list" grammar could be defined with `rest` combinator. If the
rest of the list is a property list, `props` combinator should be
used.

```haskell
el    :: Grammar SexpGrammar (Sexp :- a)  b       -> Grammar SeqGrammar a b
rest  :: Grammar SexpGrammar (Sexp :- a) (b :- a) -> Grammar SeqGrammar a ([b] :- a)
props :: Grammar PropGrammar a b                  -> Grammar SeqGrammar a b
```

`props` combinator expects properties grammar `PropGrammar` which
describes keys and values to match.

```haskell
(.:)  :: Kw
      -> Grammar SexpGrammar (Sexp :- t) (a :- t)
      -> Grammar PropGrammar t (a :- t)

(.:?) :: Kw
      -> Grammar SexpGrammar (Sexp :- t) (a :- t)
      -> Grammar PropGrammar t (Maybe a :- t)
```

Please refer to Haddock for API documentation.

Diagram:

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
