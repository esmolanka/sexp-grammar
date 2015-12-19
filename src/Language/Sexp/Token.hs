{-# LANGUAGE DeriveFunctor #-}

module Language.Sexp.Token where

import Data.Text (Text)
import Data.Scientific

data Token
  = TokLParen          -- (
  | TokRParen          -- )
  | TokLBracket        -- [
  | TokRBracket        -- ]
  | TokDot             -- e.g. (foo . bar)
  | TokQuote           -- e.g. '(foo bar)
  | TokHash            -- e.g. #(foo bar)
  | TokSymbol  { getSymbol  :: Text }        -- foo, bar
  | TokKeyword { getKeyword :: Text }        -- :foo, :bar
  | TokInt     { getInt     :: Integer }     -- 42, -1, +100500
  | TokReal    { getReal    :: Scientific }  -- 42.0, -1.0, 3.14, -1e10
  | TokStr     { getString  :: Text }        -- "foo", "", "hello world"
  | TokBool    { getBool    :: Bool }        -- #f, #t
  | TokUnknown {getUnknown  :: Char }        -- for unknown lexemes
    deriving (Show, Eq)

data Position = Position
  { posFileName :: String
  , posLine     :: Int
  , posCol      :: Int
  } deriving (Show, Ord, Eq)

data LocatedBy p a = L !p !a
  deriving (Show, Eq, Functor)

{-# INLINE mapPosition #-}
mapPosition :: (p -> p') -> LocatedBy p a -> LocatedBy p' a
mapPosition f (L p a) = L (f p) a

extract :: LocatedBy p a -> a
extract (L _ a) = a
