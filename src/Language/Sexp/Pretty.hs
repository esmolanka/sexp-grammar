{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Pretty
  ( printSexpCompact
  , printSexp
  ) where

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

ppList :: (Sexp -> Doc) -> [Sexp] -> [Doc]
ppList _ [] = ["#empty"]
ppList f (x:[]) =
  case x of
    Fix Nil -> []
    _       -> ["#unit", f x]
ppList f (x:y:[]) =
  case y of
    Fix Nil -> [f x]
    _       -> [f x, dot, f y]
ppList f (x:xs)   = f x : ppList f xs


ppSexpCompact :: Sexp -> Doc
ppSexpCompact (Fix (Atom a))    = ppAtom a
ppSexpCompact (Fix (Vector ss)) = brackets (hsep (map ppSexpCompact ss))
ppSexpCompact (Fix (List ss))   = parens   (hsep (ppList ppSexpCompact ss))
ppSexpCompact (Fix Nil)         = "nil"

printSexpCompact :: Sexp -> Lazy.Text
printSexpCompact = displayT . renderPretty 0.5 75 . ppSexpCompact

ppSexp :: Sexp -> Doc
ppSexp (Fix (Atom a))    = ppAtom a
ppSexp (Fix (Vector ss)) = brackets (align $ sep (map ppSexp ss))
ppSexp (Fix (List ss))   = parens   (align $ sep (ppList ppSexp ss))
ppSexp (Fix Nil)         = "nil"

printSexp :: Sexp -> Lazy.Text
printSexp = displayT . renderPretty 0.5 75 . ppSexp
