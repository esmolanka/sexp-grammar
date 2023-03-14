{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Pretty
  ( format
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor.Foldable (para)
import Data.Scientific
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Prettyprinter
import Prettyprinter.Internal (unsafeTextWithoutNewlines)
import qualified Prettyprinter.Render.Text as Render

import Language.Sexp.Types
import Language.Sexp.Token (escape)

instance Pretty Atom where
  pretty = \case
    AtomNumber a
      | isInteger a -> pretty $ formatScientific Fixed (Just 0) a
      | otherwise   -> pretty $ formatScientific Fixed Nothing $ a
    AtomString a  -> dquotes (unsafeTextWithoutNewlines . TL.toStrict . escape . TL.fromStrict $ a)
    AtomSymbol a  -> pretty a

ppList :: [(Fix SexpF, Doc ann)] -> Doc ann
ppList ls = case ls of
  ((Fix (AtomF _),_) : _) ->
    group $ align $ nest 1 $ vsep $ map snd ls
  _other ->
    group $ align $ vsep $ map snd ls

ppSexp :: Fix SexpF -> Doc ann
ppSexp = para $ \case
  AtomF a          -> pretty a
  ParenListF ss    -> parens $ ppList ss
  BracketListF ss  -> brackets $ ppList ss
  BraceListF ss    -> braces $ ppList ss
  ModifiedF q a    ->
    case q of
      Quote    -> "'"  <> snd a
      Backtick -> "`"  <> snd a
      Comma    -> ","  <> snd a
      CommaAt  -> ",@" <> snd a
      Hash     -> "#"  <> snd a

instance Pretty (Fix SexpF) where
  pretty = ppSexp

-- | Serialize a 'Sexp' into a pretty-printed string
format :: Fix SexpF -> ByteString
format =
  encodeUtf8 .
    Render.renderLazy .
      layoutSmart (LayoutOptions (AvailablePerLine 79 0.75)) .
        ppSexp
