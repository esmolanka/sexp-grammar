{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Token where

import Data.Text (Text)
import Data.Scientific
import Data.Text.Prettyprint.Doc

data Token
  = TokLParen          -- (
  | TokRParen          -- )
  | TokLBracket        -- [
  | TokRBracket        -- ]
  | TokLBrace          -- {
  | TokRBrace          -- }
  | TokQuote           -- e.g. '(foo bar)
  | TokNumber  { getNumber  :: !Scientific }  -- 42.0, -1.0, 3.14, -1e10
  | TokString  { getString  :: !Text }        -- "foo", "", "hello world"
  | TokSymbol  { getSymbol  :: !Text }        -- foo, bar
  | TokUnknown { getUnknown :: !Char }        -- for unknown lexemes
    deriving (Show, Eq)

instance Pretty Token where
  pretty TokLParen      = "left paren '('"
  pretty TokRParen      = "right paren ')'"
  pretty TokLBracket    = "left bracket '['"
  pretty TokRBracket    = "right bracket '['"
  pretty TokLBrace      = "left brace '{'"
  pretty TokRBrace      = "right brace '}'"
  pretty TokQuote       = "quote \"'\""
  pretty (TokSymbol s)  = "symbol" <+> squote <> pretty s <> squote
  pretty (TokNumber n)  = "number" <+> pretty (show n)
  pretty (TokString s)  = "string" <+> pretty (show s)
  pretty (TokUnknown u) = "unrecognized lexeme" <+> pretty (show u)
