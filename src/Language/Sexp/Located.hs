{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Sexp.Located
  (
  -- * Parse and print
    decode
  , parseSexp
  , parseSexps
  , parseSexpWithPos
  , parseSexpsWithPos
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
  , LocatedBy (..)
  , Position (..)
  , Compose (..)
  , Fix (..)
  , dummyPos
  -- * Conversion
  , fromSimple
  , toSimple
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Functor.Compose
import Data.Functor.Foldable (Fix (..))
import Data.Scientific (Scientific)
import Data.Text (Text)

import Language.Sexp.Types
import Language.Sexp.Lexer (lexSexp)
import Language.Sexp.Parser (parseSexp_, parseSexps_)
import qualified Language.Sexp.Pretty as Internal
import qualified Language.Sexp.Encode as Internal

-- | S-expression type annotated with positions. Useful for further
-- parsing.
type Sexp = Fix (Compose (LocatedBy Position) SexpF)

instance {-# OVERLAPPING #-} Show Sexp where
  show = unpack . encode

-- | Deserialise a 'Sexp' from a string
decode :: ByteString -> Either String Sexp
decode = parseSexp "<string>"

-- | Serialise a 'Sexp' into a compact string
encode :: Sexp -> ByteString
encode = Internal.encode . stripLocation

-- | Serialise a 'Sexp' into a pretty-printed string
format :: Sexp -> ByteString
format = Internal.format . stripLocation

----------------------------------------------------------------------

fromSimple :: Fix SexpF -> Fix (Compose (LocatedBy Position) SexpF)
fromSimple = addLocation dummyPos

toSimple :: Fix (Compose (LocatedBy Position) SexpF) -> Fix SexpF
toSimple = stripLocation

----------------------------------------------------------------------

pattern Atom :: Atom -> Sexp
pattern Atom a <- Fix (Compose (_ :< AtomF a))
  where Atom a =  Fix (Compose (dummyPos :< AtomF a))

pattern Number :: Scientific -> Sexp
pattern Number a <- Fix (Compose (_ :< AtomF (AtomNumber a)))
  where Number a =  Fix (Compose (dummyPos :< AtomF (AtomNumber a)))

pattern Symbol :: Text -> Sexp
pattern Symbol a <- Fix (Compose (_ :< AtomF (AtomSymbol a)))
  where Symbol a =  Fix (Compose (dummyPos :< AtomF (AtomSymbol a)))

pattern String :: Text -> Sexp
pattern String a <- Fix (Compose (_ :< AtomF (AtomString a)))
  where String a =  Fix (Compose (dummyPos :< AtomF (AtomString a)))

pattern ParenList :: [Sexp] -> Sexp
pattern ParenList ls <- Fix (Compose (_ :< ParenListF ls))
  where ParenList ls =  Fix (Compose (dummyPos :< ParenListF ls))

pattern BracketList :: [Sexp] -> Sexp
pattern BracketList ls <- Fix (Compose (_ :< BracketListF ls))
  where BracketList ls =  Fix (Compose (dummyPos :< BracketListF ls))

pattern BraceList :: [Sexp] -> Sexp
pattern BraceList ls <- Fix (Compose (_ :< BraceListF ls))
  where BraceList ls =  Fix (Compose (dummyPos :< BraceListF ls))

pattern Modified :: Prefix -> Sexp -> Sexp
pattern Modified q s <- Fix (Compose (_ :< ModifiedF q s))
  where Modified q s =  Fix (Compose (dummyPos :< ModifiedF q s))

-- | Parse a 'Sexp' from a string.
parseSexp :: FilePath -> ByteString -> Either String Sexp
parseSexp fn inp = parseSexp_ (lexSexp (Position fn 1 0) inp)

-- | Parse multiple 'Sexp' from a string.
parseSexps :: FilePath -> ByteString -> Either String [Sexp]
parseSexps fn inp = parseSexps_ (lexSexp (Position fn 1 0) inp)

-- | Parse a 'Sexp' from a string, starting from a given
-- position. Useful for embedding into other parsers.
parseSexpWithPos :: Position -> ByteString -> Either String Sexp
parseSexpWithPos pos inp = parseSexp_ (lexSexp pos inp)

-- | Parse multiple 'Sexp' from a string, starting from a given
-- position. Useful for embedding into other parsers.
parseSexpsWithPos :: Position -> ByteString -> Either String [Sexp]
parseSexpsWithPos pos inp = parseSexps_ (lexSexp pos inp)
