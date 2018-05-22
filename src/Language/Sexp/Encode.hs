{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Encode
  ( encode
  , escape
  ) where

import Data.Functor.Foldable (cata)
import Data.List (intersperse)
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.ByteString.Lazy.Builder.ASCII

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Language.Sexp.Types

escape :: T.Text -> TL.Text
escape = TL.concat . go [] . TL.fromStrict
  where
    go acc text
      | TL.null text = acc
      | otherwise =
          let (chunk, rest) = TL.break (\c -> c == '"' || c == '\\') text
          in case TL.uncons rest of
               Nothing -> chunk : acc
               Just ('"', rest') -> go (chunk : "\\\"" : acc) rest'
               Just ('\\',rest') -> go (chunk : "\\\\" : acc) rest'
               Just (other, rest') -> go (chunk : TL.singleton other : acc) rest'

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
        AtomString a    -> char8 '"' <> lazyByteString (TL.encodeUtf8 (escape a)) <> char8 '"'
        AtomSymbol a    -> byteString (T.encodeUtf8 a)
      ParenListF ss   -> char8 '(' <> hsep ss <> char8 ')'
      BracketListF ss -> char8 '[' <> hsep ss <> char8 ']'
      BraceListF ss   -> char8 '{' <> hsep ss <> char8 '}'
      QuotedF q a     -> case q of
                           Tick     -> char8 '\'' <> a
                           Backtick -> char8 '`' <> a
                           Comma    -> char8 ',' <> a

-- | Serialise a 'BareSexp' into a compact string
encode :: BareSexp -> ByteString
encode = toLazyByteString . buildSexp

instance {-# OVERLAPPING #-} Show Sexp where
  show = unpack . encode . stripLocation

instance {-# OVERLAPPING #-} Show BareSexp where
  show = unpack . encode
