{-# LANGUAGE PatternSynonyms     #-}

module Language.Sexp
  (
  -- * Parse and print
    decode
  , encode
  , parseSexp
  , parseSexps
  , parseSexpWithPos
  , parseSexpsWithPos
  , prettySexp
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
  , stripLocation
  ) where

import qualified Data.Text.Lazy as TL

import Language.Sexp.Types
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import Language.Sexp.Lexer  (lexSexp)
import Language.Sexp.Pretty (prettySexp)
import Language.Sexp.Encode (encode)

decode :: TL.Text -> Either String BareSexp
decode = fmap stripLocation . parseSexp "<str>"

parseSexp :: FilePath -> TL.Text -> Either String Sexp
parseSexp fn inp = parseSexp_ (lexSexp (Position fn 1 0) inp)

parseSexps :: FilePath -> TL.Text -> Either String [Sexp]
parseSexps fn inp = parseSexps_ (lexSexp (Position fn 1 0) inp)

parseSexpWithPos :: Position -> TL.Text -> Either String Sexp
parseSexpWithPos pos inp = parseSexp_ (lexSexp pos inp)

parseSexpsWithPos :: Position -> TL.Text -> Either String [Sexp]
parseSexpsWithPos pos inp = parseSexps_ (lexSexp pos inp)
