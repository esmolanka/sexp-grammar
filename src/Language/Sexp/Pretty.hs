{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Pretty
  ( printSexp
  , printSexps
  ) where

import Data.Functor.Foldable (Fix (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import Data.Text (Text)
import Data.Scientific

import Text.PrettyPrint.Leijen.Text

import Language.Sexp.Types

text' :: Text -> Doc
text' = text . Lazy.fromStrict

ppAtom :: Atom -> Doc
ppAtom (AtomBool a)    = if a then "#t" else "#f"
ppAtom (AtomInt a)     = integer a
ppAtom (AtomReal a)    = text'. T.pack . formatScientific Generic Nothing $ a
ppAtom (AtomString a)  = pretty (show a)
ppAtom (AtomSymbol a)  = text' a
ppAtom (AtomKeyword a) = text' a

ppSexp :: Sexp -> Doc
ppSexp (Fix (Atom a))            = ppAtom a
ppSexp (Fix (Vector ss))         = brackets (align $ sep (map ppSexp ss))
ppSexp (Fix (Quoted a))          = squote <> ppSexp a
ppSexp (Fix (List ss (Fix Nil))) = parens (align $ sep (map ppSexp (NE.toList ss)))
ppSexp (Fix (List ss other))     = parens (align $ sep (map ppSexp (NE.toList ss)) <+> dot <+> ppSexp other)
ppSexp (Fix Nil)                 = parens empty

printSexp :: Sexp -> Lazy.Text
printSexp = displayT . renderPretty 0.5 75 . ppSexp

printSexps :: [Sexp] -> Lazy.Text
printSexps = displayT . renderPretty 0.5 75 . vsep . map ppSexp
