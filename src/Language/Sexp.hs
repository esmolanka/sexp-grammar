{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp
  (
  -- * Parse and print
    decode
  , decodeMany
  , encode
  , format
  -- * Type
  , Sexp
  , pattern Atom
  , pattern Number
  , pattern Symbol
  , pattern String
  , pattern ParenList
  , pattern BracketList
  , pattern BraceList
  , pattern Modified
  -- ** Internal types
  , SexpF (..)
  , Atom (..)
  , Prefix (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Text (Text)
import Data.Scientific (Scientific)

import Language.Sexp.Types
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import Language.Sexp.Lexer  (lexSexp)
import qualified Language.Sexp.Pretty as Internal
import qualified Language.Sexp.Encode as Internal

type Sexp = Fix SexpF

instance {-# OVERLAPPING #-} Show Sexp where
  show = unpack . encode

-- | Deserialise a 'Sexp' from a string
decode :: ByteString -> Either String Sexp
decode = fmap stripLocation . parseSexp_ . lexSexp (Position "<string>" 1 0)

-- | Deserialise potentially multiple 'Sexp' from a string
decodeMany :: ByteString -> Either String [Sexp]
decodeMany = fmap (fmap stripLocation) . parseSexps_ . lexSexp (Position "<string>" 1 0)

-- | Serialise a 'Sexp' into a compact string
encode :: Sexp -> ByteString
encode = Internal.encode

-- | Serialise a 'Sexp' into a pretty-printed string
format :: Sexp -> ByteString
format = Internal.format

----------------------------------------------------------------------

pattern Atom :: Atom -> Sexp
pattern Atom a = Fix (AtomF a)

pattern Number :: Scientific -> Sexp
pattern Number a = Fix (AtomF (AtomNumber a))

pattern Symbol :: Text -> Sexp
pattern Symbol a = Fix (AtomF (AtomSymbol a))

pattern String :: Text -> Sexp
pattern String a = Fix (AtomF (AtomString a))

pattern ParenList :: [Sexp] -> Sexp
pattern ParenList ls = Fix (ParenListF ls)

pattern BracketList :: [Sexp] -> Sexp
pattern BracketList ls = Fix (BracketListF ls)

pattern BraceList :: [Sexp] -> Sexp
pattern BraceList ls = Fix (BraceListF ls)

pattern Modified :: Prefix -> Sexp -> Sexp
pattern Modified q s = Fix (ModifiedF q s)
