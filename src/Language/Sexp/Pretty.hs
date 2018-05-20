{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Pretty
  ( prettySexp
  , prettySexps
  ) where

import Data.Functor.Foldable (cata)
import Data.Scientific
import qualified Data.Text.Lazy as Lazy
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Render

import Language.Sexp.Types

instance Pretty Atom where
  pretty = \case
    AtomNumber a
      | isInteger a -> pretty $ formatScientific Fixed (Just 0) a
      | otherwise   -> pretty $ formatScientific Fixed Nothing $ a
    AtomString a  -> pretty (show a)
    AtomSymbol a  -> pretty a


ppList :: [Doc ann] -> Doc ann
ppList ls = group $ align $ nest 2 $ vsep ls

ppSexp :: Fix SexpF -> Doc ann
ppSexp = cata $ \case
  AtomF a         -> pretty a
  ParenListF ss   -> parens $ ppList ss
  BracketListF ss -> brackets $ ppList ss
  BraceListF ss   -> braces $ ppList ss
  QuotedF a       -> squote <> a

instance Pretty (Fix SexpF) where
  pretty = ppSexp

-- | Pretty-print a Sexp to a Text
prettySexp :: Sexp -> Lazy.Text
prettySexp = renderDoc . ppSexp . extractRecursive

-- | Pretty-print a list of Sexps as a sequence of S-expressions to a ByteString
prettySexps :: [Sexp] -> Lazy.Text
prettySexps = renderDoc . vcat . punctuate (line <> line) . map (ppSexp  . extractRecursive)

renderDoc :: Doc ann -> Lazy.Text
renderDoc = Render.renderLazy . layoutSmart (LayoutOptions (AvailablePerLine 79 0.75))
