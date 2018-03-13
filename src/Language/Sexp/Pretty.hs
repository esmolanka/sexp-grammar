{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Pretty
  ( prettySexp'
  , prettySexp
  , prettySexps
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Monoid as Monoid
import Data.Scientific
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Render

import Language.Sexp.Types

instance Pretty Kw where
  pretty (Kw s) = colon <> pretty s

ppAtom :: Atom -> Doc ann
ppAtom (AtomBool a)    = if a then "#t" else "#f"
ppAtom (AtomInt a)     = pretty a
ppAtom (AtomReal a)    = pretty $ formatScientific Generic Nothing $ a
ppAtom (AtomString a)  = pretty (show a)
ppAtom (AtomSymbol a)  = pretty a
ppAtom (AtomKeyword k) = pretty k

instance Pretty Atom where
  pretty = ppAtom

ppList :: [Sexp] -> Doc ann
ppList ls =
  align $ case ls of
    [] ->
      Monoid.mempty
    a : [] ->
      ppSexp a
    a : b : [] ->
      ppSexp a <+> ppSexp b
    a : rest@(_ : _ : _) ->
      ppSexp a <+> group (nest 2 (vsep (map ppSexp rest)))

ppSexp :: Sexp -> Doc ann
ppSexp (Atom   _ a)  = ppAtom a
ppSexp (List   _ ss) = parens $ ppList ss
ppSexp (Vector _ ss) = brackets $ ppList ss
ppSexp (Quoted _ a)  = squote <> ppSexp a

instance Pretty Sexp where
  pretty = ppSexp

-- | Pretty-print a Sexp to a Text
prettySexp :: Sexp -> Lazy.Text
prettySexp = renderDoc . ppSexp

-- | Pretty-print a Sexp to a ByteString
prettySexp' :: Sexp -> ByteString
prettySexp' = encodeUtf8 . prettySexp

-- | Pretty-print a list of Sexps as a sequence of S-expressions to a ByteString
prettySexps :: [Sexp] -> Lazy.Text
prettySexps = renderDoc . vcat . punctuate (line <> line) . map ppSexp

renderDoc :: Doc ann -> Lazy.Text
renderDoc = Render.renderLazy . layoutPretty (LayoutOptions (AvailablePerLine 79 0.75))
