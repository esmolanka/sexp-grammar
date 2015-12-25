{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.Sexp.Token where

import Data.Text (Text)
import Data.Text.Lazy (pack, fromStrict)
import Data.Scientific

import Text.PrettyPrint.Leijen.Text

data Token
  = TokLParen          -- (
  | TokRParen          -- )
  | TokLBracket        -- [
  | TokRBracket        -- ]
  | TokQuote           -- e.g. '(foo bar)
  | TokHash            -- e.g. #(foo bar)
  | TokSymbol  { getSymbol  :: !Text }        -- foo, bar
  | TokKeyword { getKeyword :: !Text }        -- :foo, :bar
  | TokInt     { getInt     :: !Integer }     -- 42, -1, +100500
  | TokReal    { getReal    :: !Scientific }  -- 42.0, -1.0, 3.14, -1e10
  | TokStr     { getString  :: !Text }        -- "foo", "", "hello world"
  | TokBool    { getBool    :: !Bool }        -- #f, #t
  | TokUnknown { getUnknown :: !Char }        -- for unknown lexemes
    deriving (Show, Eq)

data Position =
  Position !Int !Int
  deriving (Show, Ord, Eq)

data LocatedBy p a = L !p !a
  deriving (Show, Eq, Functor)

{-# INLINE mapPosition #-}
mapPosition :: (p -> p') -> LocatedBy p a -> LocatedBy p' a
mapPosition f (L p a) = L (f p) a

extract :: LocatedBy p a -> a
extract (L _ a) = a

instance Pretty Token where
  pretty TokLParen      = "left paren '('"
  pretty TokRParen      = "right paren ')'"
  pretty TokLBracket    = "left bracket '['"
  pretty TokRBracket    = "right bracket '['"
  pretty TokQuote       = "quote \"'\""
  pretty TokHash        = "hash '#'"
  pretty (TokSymbol s)  = "symbol" <+> dquote <> text (fromStrict s) <> dquote
  pretty (TokKeyword k) = "keyword" <+> dquote <> text (fromStrict k) <> dquote
  pretty (TokInt     n) = "integer" <+> integer n
  pretty (TokReal    n) = "real number" <+> text (pack (show n))
  pretty (TokStr     s) = "string" <+> text (pack (show s))
  pretty (TokBool    b) = "boolean" <+> if b then "#t" else "#f"
  pretty (TokUnknown u) = "unknown lexeme" <+> text (pack (show u))

instance Pretty Position where
  pretty (Position line col) = int line <> colon <> int col
