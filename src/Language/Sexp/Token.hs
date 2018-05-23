{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Token
  ( Token (..)
  , Prefix (..)
  ) where

import Data.Text (Text)
import Data.Scientific
import Data.Text.Prettyprint.Doc

import Language.Sexp.Types (Prefix(..))

data Token
  = TokLParen          -- (
  | TokRParen          -- )
  | TokLBracket        -- [
  | TokRBracket        -- ]
  | TokLBrace          -- {
  | TokRBrace          -- }
  | TokPrefix  { getPrefix  :: !Prefix }      -- e.g. a quote in '(foo bar)
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
  pretty (TokPrefix c)  = "modifier" <+> pretty (show c)
  pretty (TokSymbol s)  = "symbol" <+> squotes (pretty s) <> squote
  pretty (TokNumber n)  = "number" <+> pretty (show n)
  pretty (TokString s)  = "string" <+> pretty (show s)
  pretty (TokUnknown u) = "unrecognized" <+> pretty (show u)
