sexp-grammar
============

Library of invertible parsing combinators for S-expressions. The
combinators define primitive grammars and ways to compose them. A
grammar constructed with these combinators can be run in two
directions: parsing from S-expressions direction (forward) and
serialising to S-expressions direction (backward).

The approach used in `sexp-grammar` is inspired by the paper
[Invertible syntax descriptions: Unifying parsing and pretty printing](http://www.informatik.uni-marburg.de/~rendel/unparse/)
and a similar implementation of invertible grammar approach for JSON, library by
Martijn van Steenbergen called [JsonGrammar2](https://github.com/MedeaMelana/JsonGrammar2).

See [`sexp-grammar`](http://github.com/esmolanka/sexp-grammar).
