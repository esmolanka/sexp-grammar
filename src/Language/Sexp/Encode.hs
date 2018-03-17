{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Encode
  ( encode
  ) where

import Data.List (intersperse)
import Data.Monoid as Monoid
import Data.Scientific
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder.ASCII

import Language.Sexp.Types

bAtom :: Atom -> Builder
bAtom (AtomBool a)    = char8 '#' <> if a then char8 't' else char8 'f'
bAtom (AtomInt a)     = integerDec a
bAtom (AtomReal a)    = string8 . formatScientific Generic Nothing $ a
bAtom (AtomString a)  = stringUtf8 (show a)
bAtom (AtomSymbol a)  = byteString (encodeUtf8 a)
bAtom (AtomKeyword a) = char8 ':' <> byteString (encodeUtf8 (unKw a))

sep :: [Builder] -> Builder
sep = mconcat . intersperse (char8 ' ')

bSexp :: Sexp -> Builder
bSexp (Atom   _ a)  = bAtom a
bSexp (List   _ ss) = char8 '(' <> sep (map bSexp ss) <> char8 ')'
bSexp (Vector _ ss) = char8 '[' <> sep (map bSexp ss) <> char8 ']'
bSexp (Quoted _ a)  = char8 '\'' Monoid.<> bSexp a

-- | Quickly encode Sexp to non-indented ByteString
encode :: Sexp -> ByteString
encode = toLazyByteString . bSexp
