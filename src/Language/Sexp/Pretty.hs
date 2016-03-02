{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Pretty
  ( prettySexp
  , prettySexps
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Scientific
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Text.PrettyPrint.Leijen.Text

import Language.Sexp.Types

instance Pretty Kw where
  pretty = text . Lazy.fromStrict . unKw

ppAtom :: Atom -> Doc
ppAtom (AtomBool a)    = if a then "#t" else "#f"
ppAtom (AtomInt a)     = integer a
ppAtom (AtomReal a)    = text . Lazy.pack . formatScientific Generic Nothing $ a
ppAtom (AtomString a)  = pretty (show a)
ppAtom (AtomSymbol a)  = text . Lazy.fromStrict $ a
ppAtom (AtomKeyword k) = colon <> pretty k

instance Pretty Atom where
  pretty = ppAtom

ppSexp :: Sexp -> Doc
ppSexp (Atom   _ a)  = ppAtom a
ppSexp (Vector _ ss) = brackets (align $ sep (map ppSexp ss))
ppSexp (Quoted _ a)  = squote <> ppSexp a
ppSexp (List   _ ss) = parens (align $ sep (map ppSexp ss))

instance Pretty Sexp where
  pretty = ppSexp

-- | Pretty-print a Sexp to a ByteString
prettySexp :: Sexp -> ByteString
prettySexp = encodeUtf8 . displayT . renderPretty 0.5 75 . ppSexp

-- | Pretty-print a list of Sexps as a sequence of S-expressions to a ByteString
prettySexps :: [Sexp] -> ByteString
prettySexps = encodeUtf8 . displayT . renderPretty 0.5 75 . vcat . punctuate (line <> line) . map ppSexp
