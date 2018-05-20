{-# LANGUAGE PatternSynonyms     #-}

module Language.Sexp
  (
  -- * Parse and print
    decode
  , encode
  , parseSexp
  , parseSexps
  , parseSexp'
  , parseSexps'
  , prettySexp
  , prettySexps
  -- * Type
  , BareSexp
  , Sexp
  , pattern Atom
  , pattern Number
  , pattern Symbol
  , pattern String
  , pattern ParenList
  , pattern BracketList
  , pattern BraceList
  , pattern Quoted
  , SexpF (..)
  , Atom (..)
  -- ** Position
  , Position (..)
  , dummyPos
  , LocatedBy (..)
  , location
  , extract
  , extractRecursive
  ) where

import qualified Data.Text.Lazy as TL

import Language.Sexp.Types
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import Language.Sexp.Lexer  (lexSexp)
import Language.Sexp.Pretty (prettySexp, prettySexps)
import Language.Sexp.Encode (encode)

decode :: TL.Text -> Either String BareSexp
decode = fmap extractRecursive . parseSexp "<str>"

parseSexp :: FilePath -> TL.Text -> Either String Sexp
parseSexp fn inp = parseSexp_ (lexSexp (Position fn 1 0) inp)

parseSexps :: FilePath -> TL.Text -> Either String [Sexp]
parseSexps fn inp = parseSexps_ (lexSexp (Position fn 1 0) inp)

parseSexp' :: Position -> TL.Text -> Either String Sexp
parseSexp' pos inp = parseSexp_ (lexSexp pos inp)

parseSexps' :: Position -> TL.Text -> Either String [Sexp]
parseSexps' pos inp = parseSexps_ (lexSexp pos inp)
