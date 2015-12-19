
module Language.Sexp
  ( parseSexpsFrom
  , parseSexpFrom
  , printSexps
  , parseSexp
  , SexpF (..)
  , Atom (..)
  ) where

import Language.Sexp.Types
import Language.Sexp.Lexer
import Language.Sexp.Parser
import Language.Sexp.Pretty

parseSexpsFrom :: FilePath -> String -> Either String [Sexp]
parseSexpsFrom fn = parseProgram . lexSexp fn

parseSexpFrom :: FilePath -> String -> Either String Sexp
parseSexpFrom fn = parseSexp . lexSexp fn
