{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Pretty
  ( printSexp
  , printSexps
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import Data.Text (Text)
import Data.Scientific

import Text.PrettyPrint.Leijen.Text

import Language.Sexp.Types

text' :: Text -> Doc
text' = text . Lazy.fromStrict

ppKw :: Kw -> Doc
ppKw (Kw kw) = colon <> text' kw

ppAtom :: Atom -> Doc
ppAtom (AtomBool a)    = if a then "#t" else "#f"
ppAtom (AtomInt a)     = integer a
ppAtom (AtomReal a)    = text'. T.pack . formatScientific Generic Nothing $ a
ppAtom (AtomString a)  = pretty (show a)
ppAtom (AtomSymbol a)  = text' a
ppAtom (AtomKeyword k) = ppKw k

ppSexp :: Sexp -> Doc
ppSexp (Atom   _ a) = ppAtom a
ppSexp (Vector _ ss) = brackets (align $ sep (map ppSexp ss))
ppSexp (Quoted _ a) = squote <> ppSexp a
ppSexp (List   _ ss) = parens (align $ sep (map ppSexp ss))

printSexp :: Sexp -> Lazy.Text
printSexp = displayT . renderPretty 0.5 75 . ppSexp

printSexps :: [Sexp] -> Lazy.Text
printSexps = displayT . renderPretty 0.5 75 . vsep . map ppSexp
