{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Pretty
  ( prettySexp'
  , prettySexp
  , prettySexps
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Scientific
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Text.PrettyPrint.Leijen.Text

import Language.Sexp.Types

instance Pretty Kw where
  pretty (Kw s) = colon <> text (Lazy.fromStrict s)

ppAtom :: Atom -> Doc
ppAtom (AtomBool a)    = if a then "#t" else "#f"
ppAtom (AtomInt a)     = integer a
ppAtom (AtomReal a)    = text . Lazy.pack . formatScientific Generic Nothing $ a
ppAtom (AtomString a)  = pretty (show a)
ppAtom (AtomSymbol a)  = text . Lazy.fromStrict $ a
ppAtom (AtomKeyword k) = pretty k

instance Pretty Atom where
  pretty = ppAtom

ppList :: [Sexp] -> Doc
ppList ls =
  align $ case ls of
    [] ->
      empty
    a : [] ->
      ppSexp a
    a : b : [] ->
      ppSexp a <+> ppSexp b
    a : rest@(_ : _ : _) ->
      ppSexp a <+> group (nest 2 (vsep (map ppSexp rest)))

ppSexp :: Sexp -> Doc
ppSexp (Atom   _ a)  = ppAtom a
ppSexp (List   _ ss) = parens $ ppList ss
ppSexp (Vector _ ss) = brackets $ ppList ss
ppSexp (Quoted _ a)  = squote <> ppSexp a

instance Pretty Sexp where
  pretty = ppSexp

-- | Pretty-print a Sexp to a Text
prettySexp' :: Sexp -> Lazy.Text
prettySexp' = displayT . renderPretty 0.75 79 . ppSexp

-- | Pretty-print a Sexp to a ByteString
prettySexp :: Sexp -> ByteString
prettySexp = encodeUtf8 . prettySexp'

-- | Pretty-print a list of Sexps as a sequence of S-expressions to a ByteString
prettySexps :: [Sexp] -> ByteString
prettySexps = encodeUtf8 . displayT . renderPretty 0.75 79 . vcat . punctuate (line <> line) . map ppSexp
