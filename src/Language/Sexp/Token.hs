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
  | TokSymbol  { getSymbol  :: !Text }        -- foo, bar
  | TokKeyword { getKeyword :: !Text }        -- :foo, :bar
  | TokInt     { getInt     :: !Integer }     -- 42, -1, +100500
  | TokReal    { getReal    :: !Scientific }  -- 42.0, -1.0, 3.14, -1e10
  | TokStr     { getString  :: !Text }        -- "foo", "", "hello world"
  | TokUnknown { getUnknown :: !Char }        -- for unknown lexemes
    deriving (Show, Eq)

data LocatedBy p a = L !p !a
  deriving (Show, Eq, Functor)

{-# INLINE mapPosition #-}
mapPosition :: (p -> p') -> LocatedBy p a -> LocatedBy p' a
mapPosition f (L p a) = L (f p) a

extract :: LocatedBy p a -> a
extract (L _ a) = a

(@@) :: (a -> (p -> b)) -> LocatedBy p a -> b
(@@) f (L p a) = f a p

instance Pretty Token where
  pretty TokLParen      = "left paren '('"
  pretty TokRParen      = "right paren ')'"
  pretty TokLBracket    = "left bracket '['"
  pretty TokRBracket    = "right bracket '['"
  pretty TokLBrace      = "left brace '{'"
  pretty TokRBrace      = "right brace '}'"
  pretty TokQuote       = "quote \"'\""
  pretty (TokSymbol s)  = "symbol" <+> dquote <> pretty s <> dquote
  pretty (TokKeyword k) = "keyword" <+> dquote <> pretty k <> dquote
  pretty (TokInt     n) = "integer" <+> pretty n
  pretty (TokReal    n) = "real number" <+> pretty (show n)
  pretty (TokStr     s) = "string" <+> pretty (show s)
  pretty (TokUnknown u) = "unknown lexeme" <+> pretty (show u)
