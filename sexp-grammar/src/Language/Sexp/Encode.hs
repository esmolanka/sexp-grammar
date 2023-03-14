{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Sexp.Encode (encode) where

import Data.Functor.Foldable (cata)
import Data.List (intersperse)
import Data.Scientific
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Builder

import Language.Sexp.Types
import Language.Sexp.Token (escape)

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
        AtomString a    -> char8 '"' <> lazyByteString (TL.encodeUtf8 (escape (TL.fromStrict a))) <> char8 '"'
        AtomSymbol a    -> byteString (T.encodeUtf8 a)
      ParenListF ss   -> char8 '(' <> hsep ss <> char8 ')'
      BracketListF ss -> char8 '[' <> hsep ss <> char8 ']'
      BraceListF ss   -> char8 '{' <> hsep ss <> char8 '}'
      ModifiedF q a   -> case q of
        Quote    -> char8 '\'' <> a
        Backtick -> char8 '`' <> a
        Comma    -> char8 ',' <> a
        CommaAt  -> char8 ',' <> char8 '@' <> a
        Hash     -> char8 '#' <> a

encode :: Fix SexpF -> ByteString
encode = toLazyByteString . buildSexp
