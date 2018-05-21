{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.LexerInterface
  ( LineCol(..)
  , AlexInput(..)
  , mkAlexInput
  -- Alex interfare
  , alexInputPrevChar
  , alexGetByte
  ) where

import Data.Int
import Data.ByteString.Lazy (ByteString, uncons)
import Data.ByteString.Lazy.UTF8 (decode)
import Data.Word (Word8)

data LineCol = LineCol {-# UNPACK #-} !Int {-# UNPACK #-} !Int

columnsInTab :: Int
columnsInTab = 8

advanceLineCol :: Char -> LineCol -> LineCol
advanceLineCol '\n' (LineCol line _)   = LineCol (line + 1) 0
advanceLineCol '\t' (LineCol line col) = LineCol line (((col + columnsInTab - 1) `div` columnsInTab) * columnsInTab + 1)
advanceLineCol _    (LineCol line col) = LineCol line (col + 1)

data AlexInput = AlexInput
  { aiInput     :: ByteString
  , aiPrevChar  :: {-# UNPACK #-} !Char
  , aiCurChar   :: {-# UNPACK #-} !Char
  , aiBytesLeft :: {-# UNPACK #-} !Int64
  , aiLineCol   :: !LineCol
  }

mkAlexInput :: LineCol -> ByteString -> AlexInput
mkAlexInput initPos source = alexNextChar $ AlexInput
  { aiInput     = source
  , aiPrevChar  = '\n'
  , aiCurChar   = '\n'
  , aiBytesLeft = 0
  , aiLineCol   = initPos
  }

alexNextChar :: AlexInput -> AlexInput
alexNextChar input =
  case decode (aiInput input) of
    Just (c, n) -> input
      { aiPrevChar  = aiCurChar input
      , aiCurChar   = c
      , aiBytesLeft = n
      }
    Nothing     -> input
      { aiPrevChar  = aiCurChar input
      , aiCurChar   = '\n'
      , aiBytesLeft = 0
      }

alexPropagatePos :: AlexInput -> AlexInput
alexPropagatePos input =
  input { aiLineCol = advanceLineCol (aiPrevChar input) (aiLineCol input) }

-- Alex interface - functions usedby Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input
  | aiBytesLeft input == 0 = go . alexPropagatePos . alexNextChar $ input
  | otherwise = go input
  where
    go :: AlexInput -> Maybe (Word8, AlexInput)
    go input =
      case uncons (aiInput input) of
        Just (w, rest) -> Just (w, input
          { aiBytesLeft = aiBytesLeft input - 1
          , aiInput     = rest
          })
        Nothing -> Nothing

