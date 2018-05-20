{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Encode
  ( encode
  ) where

import Data.Functor.Foldable (cata)
import Data.List (intersperse)
import Data.Scientific
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.ByteString.Lazy.Builder.ASCII

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Language.Sexp.Types

buildSexp :: Fix SexpF -> Builder
buildSexp = cata alg
  where
    hsep :: [Builder] -> Builder
    hsep = mconcat . intersperse (char8 ' ')

    alg :: SexpF Builder -> Builder
    alg = \case
      AtomF atom -> case atom of
        AtomNumber a
          | isInteger a -> string8 (formatScientific Fixed (Just 0) a)
          | otherwise   -> string8 (formatScientific Fixed Nothing a)
        AtomString a    -> stringUtf8 (show a)
        AtomSymbol a    -> byteString (encodeUtf8 a)
      ParenListF ss   -> char8 '(' <> hsep ss <> char8 ')'
      BracketListF ss -> char8 '[' <> hsep ss <> char8 ']'
      BraceListF ss   -> char8 '{' <> hsep ss <> char8 '}'
      QuotedF a       -> char8 '\'' <> a

encode :: BareSexp -> ByteString
encode = toLazyByteString . buildSexp

instance {-# OVERLAPPING #-} Show Sexp where
  show = unpack . encode . stripLocation

instance {-# OVERLAPPING #-} Show BareSexp where
  show = unpack . encode
