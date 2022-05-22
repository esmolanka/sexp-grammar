{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Token
  ( Token (..)
  , Prefix (..)
  , escape
  , unescape
  ) where

import Data.Scientific
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif

import Language.Sexp.Types (Prefix(..))

data Token
  = TokLParen          -- (
  | TokRParen          -- )
  | TokLBracket        -- [
  | TokRBracket        -- ]
  | TokLBrace          -- {
  | TokRBrace          -- }
  | TokCommentIntro    -- #;
  | TokPrefix  { getPrefix  :: !Prefix }      -- e.g. a quote in '(foo bar)
  | TokNumber  { getNumber  :: !Scientific }  -- 42.0, -1.0, 3.14, -1e10
  | TokString  { getString  :: !Text }        -- "foo", "", "hello world"
  | TokSymbol  { getSymbol  :: !Text }        -- foo, bar
  | TokUnknown { getUnknown :: !Text }        -- for unknown lexemes
  | TokEOF
    deriving (Show, Eq)

instance Pretty Token where
  pretty TokLParen       = "left paren '('"
  pretty TokRParen       = "right paren ')'"
  pretty TokLBracket     = "left bracket '['"
  pretty TokRBracket     = "right bracket '['"
  pretty TokLBrace       = "left brace '{'"
  pretty TokRBrace       = "right brace '}'"
  pretty TokCommentIntro = "datum comment"
  pretty (TokPrefix c)   = "modifier" <+> pretty (show c)
  pretty (TokSymbol s)   = "symbol" <+> squotes (pretty s) <> squote
  pretty (TokNumber n)   = "number" <+> pretty (show n)
  pretty (TokString s)   = "string" <+> pretty (show s)
  pretty (TokUnknown u)  = "unrecognized" <+> pretty u <> "..."
  pretty TokEOF          = "end of file"


newtype DText = DText (TL.Text -> TL.Text)

instance Semigroup DText where
  DText a <> DText b = DText (a . b)

instance Monoid DText where
  mempty = DText id
  mappend = (<>)

delay :: TL.Text -> DText
delay t = DText (t `TL.append`)

force :: DText -> TL.Text
force (DText f) = f TL.empty


unescape :: TL.Text -> TL.Text
unescape = force . go mempty
  where
    go :: DText -> TL.Text -> DText
    go acc text
      | TL.null text = acc
      | otherwise =
         let (chunk, rest) = TL.break (== '\\') text in
         case TL.uncons rest of
           Nothing -> acc <> delay chunk
           Just (_, rest') ->
             case TL.uncons rest' of
               Nothing -> error "Invalid escape sequence"
               Just ('n', rest'') -> go (acc <> delay (chunk `TL.snoc` '\n')) rest''
               Just ('r', rest'') -> go (acc <> delay (chunk `TL.snoc` '\r')) rest''
               Just ('t', rest'') -> go (acc <> delay (chunk `TL.snoc` '\t')) rest''
               Just (lit, rest'') -> go (acc <> delay (chunk `TL.snoc` lit )) rest''


escape :: TL.Text -> TL.Text
escape = force . go mempty
  where
    go :: DText -> TL.Text -> DText
    go acc text
      | TL.null text = acc
      | otherwise =
          let (chunk, rest) = TL.break (\c -> c == '"' || c == '\\') text
          in case TL.uncons rest of
               Nothing -> acc <> delay chunk
               Just ('"', rest') -> go (acc <> delay chunk <> delay "\\\"") rest'
               Just ('\\',rest') -> go (acc <> delay chunk <> delay "\\\\") rest'
               Just (other, rest') -> go (acc <> delay chunk <> delay (TL.singleton other)) rest'
