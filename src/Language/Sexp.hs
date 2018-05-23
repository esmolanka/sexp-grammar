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
  , pattern Modified
  , SexpF (..)
  , Atom (..)
  , Prefix (..)
  -- ** Position
  , Position (..)
  , dummyPos
  , LocatedBy (..)
  , stripLocation
  ) where

import Data.ByteString.Lazy (ByteString)

import Language.Sexp.Types
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import Language.Sexp.Lexer  (lexSexp)
import Language.Sexp.Pretty (prettySexp)
import Language.Sexp.Encode (encode)

-- | Deserialise a 'BareSexp' from a string
decode :: ByteString -> Either String BareSexp
decode = fmap stripLocation . parseSexp "<string>"

-- | Parse a 'Sexp' from a string.
parseSexp :: FilePath -> ByteString -> Either String Sexp
parseSexp fn inp = parseSexp_ (lexSexp (Position fn 1 0) inp)

-- | Parse multiple 'Sexp' from a string.
parseSexps :: FilePath -> ByteString -> Either String [Sexp]
parseSexps fn inp = parseSexps_ (lexSexp (Position fn 1 0) inp)

-- | Parse a 'Sexp' from a string, starting from a given
-- position. Useful for embedding into other parsers.
parseSexpWithPos :: Position -> ByteString -> Either String Sexp
parseSexpWithPos pos inp = parseSexp_ (lexSexp pos inp)

-- | Parse multiple 'Sexp' from a string, starting from a given
-- position. Useful for embedding into other parsers.
parseSexpsWithPos :: Position -> ByteString -> Either String [Sexp]
parseSexpsWithPos pos inp = parseSexps_ (lexSexp pos inp)
