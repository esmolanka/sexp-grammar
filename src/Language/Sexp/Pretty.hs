{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Pretty
  ( prettySexp
  ) where

import Data.Functor.Foldable (para)
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


ppList :: [(Fix SexpF, Doc ann)] -> Doc ann
ppList ls = case ls of
  ((Fix (AtomF _),_) : _) ->
    group $ align $ nest 1 $ vsep $ map snd ls
  _other ->
    group $ align $ vsep $ map snd ls

ppSexp :: Fix SexpF -> Doc ann
ppSexp = para $ \case
  AtomF a         -> pretty a
  ParenListF ss   -> parens $ ppList ss
  BracketListF ss -> brackets $ ppList ss
  BraceListF ss   -> braces $ ppList ss
  QuotedF (_, a)  -> squote <> a

instance Pretty (Fix SexpF) where
  pretty = ppSexp


-- | Pretty-print a Sexp to a Text
prettySexp :: Sexp -> Lazy.Text
prettySexp =
  Render.renderLazy .
    layoutSmart (LayoutOptions (AvailablePerLine 79 0.75)) .
      ppSexp .
        stripLocation
