
module Language.Sexp
  (
  -- * Parse and print
    decode
  , encode
  , parseSexp
  , parseSexps
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

import qualified Data.ByteString.Lazy.Char8 as B8

import Language.Sexp.Types
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import Language.Sexp.Lexer  (lexSexp)
import Language.Sexp.Pretty (prettySexp, prettySexps)
import Language.Sexp.Encode (encode)

-- | Quickly decode a ByteString-formatted S-expression into Sexp structure
decode :: B8.ByteString -> Either String Sexp
decode = parseSexp "<bytestring>"

-- | Parse a ByteString-formatted S-expression into Sexp
-- structure. Takes file name for better error messages.
parseSexp :: FilePath -> B8.ByteString -> Either String Sexp
parseSexp fn inp =
  case parseSexp_ (lexSexp fn inp) of
    Left err -> Left $ fn ++ ":" ++ err
    Right a  -> Right a

-- | Parse a ByteString-formatted sequence of S-expressions into list
-- of Sexp structures. Takes file name for better error messages.
parseSexps :: FilePath -> B8.ByteString -> Either String [Sexp]
parseSexps fn inp =
  case parseSexps_ (lexSexp fn inp) of
    Left err -> Left $ fn ++ ":" ++ err
    Right a  -> Right a
