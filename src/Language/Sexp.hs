
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
  , Sexp (..)
  , Atom (..)
  , Kw (..)
  -- ** Position
  , Position (..)
  , dummyPos
  , getPos
  ) where

import qualified Data.Text.Lazy as TL

import Language.Sexp.Types
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import Language.Sexp.Lexer  (lexSexp)
import Language.Sexp.Pretty (prettySexp, prettySexps)
import Language.Sexp.Encode (encode)

-- | Quickly decode a ByteString-formatted S-expression into Sexp structure
decode :: TL.Text -> Either String Sexp
decode = parseSexp "<str>"

-- | Parse a ByteString-formatted S-expression into Sexp
-- structure. Takes file name for better error messages.
parseSexp :: FilePath -> TL.Text -> Either String Sexp
parseSexp fn inp = parseSexp_ (lexSexp (Position fn 1 0) inp)

-- | Parse a ByteString-formatted sequence of S-expressions into list
-- of Sexp structures. Takes file name for better error messages.
parseSexps :: FilePath -> TL.Text -> Either String [Sexp]
parseSexps fn inp = parseSexps_ (lexSexp (Position fn 1 0) inp)

-- | Parse a ByteString-formatted S-expression into Sexp
-- structure. Takes file name for better error messages.
parseSexp' :: Position -> TL.Text -> Either String Sexp
parseSexp' pos inp = parseSexp_ (lexSexp pos inp)

-- | Parse a ByteString-formatted sequence of S-expressions into list
-- of Sexp structures. Takes file name for better error messages.
parseSexps' :: Position -> TL.Text -> Either String [Sexp]
parseSexps' pos inp = parseSexps_ (lexSexp pos inp)
