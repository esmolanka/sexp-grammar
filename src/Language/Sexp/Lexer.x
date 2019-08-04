{
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-tabs               #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}

module Language.Sexp.Lexer
  ( lexSexp
  ) where

import Data.Bifunctor
import qualified Data.ByteString.Lazy as BLW
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Int
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read
import Data.Word

import Language.Sexp.Token
import Language.Sexp.Types (Position (..), LocatedBy (..))

}

$hspace     = [\ \t]
$whitespace = [$hspace\n\r\f\v]

$allgraphic = . # [\x00-\x20 \x7F-\xA0]

$digit      = 0-9
$hex        = [0-9 A-F a-f]
$alpha      = [a-z A-Z]

@number     = [\-\+]? $digit+ ([\.]$digit+)? ([eE] [\-\+]? $digit+)?

@escape     = \\ [nrt\\\"]
@string     = $allgraphic # [\"\\] | $whitespace | @escape

$unicode    = $allgraphic # [\x20-\x80]

$syminitial = [$alpha \:\@\!\$\%\&\*\/\<\=\>\?\~\_\^\.\|\+\- $unicode]
$symsubseq  = [$syminitial $digit \#\'\`\,]
@symescape  = \\ [$alpha $digit \(\)\[\]\{\}\\\|\;\'\`\"\#\.\,]
@symbol     = ($syminitial | @symescape) ($symsubseq | @symescape)*

:-

$whitespace+       ;
";" .*             ;

"("                { just TokLParen   }
")"                { just TokRParen   }
"["                { just TokLBracket }
"]"                { just TokRBracket }
"{"                { just TokLBrace   }
"}"                { just TokRBrace   }

"'" / $allgraphic  { just (TokPrefix Quote)    }
"`" / $allgraphic  { just (TokPrefix Backtick) }
",@" / $allgraphic { just (TokPrefix CommaAt)  }
"," / $allgraphic  { just (TokPrefix Comma)    }
"#" / $allgraphic  { just (TokPrefix Hash)     }

@number            { TokNumber  `via` readNum    }
@symbol            { TokSymbol  `via` decode     }
\" @string* \"     { TokString  `via` readString }

{

----------------------------------------------------------------------
-- Actions

just :: Token -> AlexAction
just tok _ = tok

via :: (a -> Token) -> (ByteString -> a) -> AlexAction
via ftok f = ftok . f

----------------------------------------------------------------------
-- Decoders

readString :: ByteString -> T.Text
readString = TL.toStrict . unescape . TL.tail . TL.init . decodeUtf8

readNum :: ByteString -> Scientific
readNum = read . TL.unpack . decodeUtf8

decode :: ByteString -> T.Text
decode = TL.toStrict . decodeUtf8

----------------------------------------------------------------------
-- Entry point

lexSexp :: Position -> ByteString -> [LocatedBy Position Token]
lexSexp (Position fn line1 col1) =
  map (bimap fixPos id) . alexScanTokens . mkAlexInput (LineCol line1 col1)
  where
    fixPos (LineCol l c) = Position fn l c

----------------------------------------------------------------------
-- Machinery

type AlexAction = ByteString -> Token

alexScanTokens :: AlexInput -> [LocatedBy LineCol Token]
alexScanTokens input =
  case alexScan input defaultCode of
    AlexEOF ->
      [aiLineCol input :< TokEOF]
    AlexError (AlexInput {aiInput, aiLineCol}) ->
      let rest = T.takeWhile (/= '\n') $ decode $ UTF8.take 100 aiInput
      in [aiLineCol :< (TokUnknown rest)]
    AlexSkip input _ ->
      alexScanTokens input
    AlexToken input' tokLen action ->
      let inputText = UTF8.take (fromIntegral tokLen) (aiInput input)
      in (aiLineCol input :< action inputText) : alexScanTokens input'
  where
    defaultCode :: Int
    defaultCode = 0

data LineCol = LineCol {-# UNPACK #-} !Int {-# UNPACK #-} !Int

columnsInTab :: Int
columnsInTab = 8

advanceLineCol :: Char -> LineCol -> LineCol
advanceLineCol '\n' (LineCol line _)   = LineCol (line + 1) 1
advanceLineCol '\t' (LineCol line col) = LineCol line (((col + columnsInTab - 1) `div` columnsInTab) * columnsInTab + 1)
advanceLineCol _    (LineCol line col) = LineCol line (col + 1)

data AlexInput = AlexInput
  { aiInput     :: ByteString
  , aiPrevChar  :: {-# UNPACK #-} !Char
  , aiBytesLeft :: {-# UNPACK #-} !Int64
  , aiLineCol   :: !LineCol
  }

mkAlexInput :: LineCol -> ByteString -> AlexInput
mkAlexInput initPos source = AlexInput
  { aiInput     = source
  , aiPrevChar  = '\n'
  , aiBytesLeft = 0
  , aiLineCol   = initPos
  }

alexNextChar :: AlexInput -> Maybe AlexInput
alexNextChar input
  | aiBytesLeft input > 1 = Just $ input { aiBytesLeft = aiBytesLeft input - 1 }
  | otherwise = case UTF8.decode (aiInput input) of
      Just (c, n) -> Just $ input
        { aiPrevChar  = c
        , aiLineCol   = advanceLineCol c (aiLineCol input)
        , aiBytesLeft = n
        }
      Nothing -> Nothing

-- Alex interface - functions used by Alex
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
  alexNextChar input >>= getByte
  where
    getByte :: AlexInput -> Maybe (Word8, AlexInput)
    getByte input =
      case BLW.uncons (aiInput input) of
        Just (w, rest) -> Just (w, input { aiInput = rest })
        Nothing -> Nothing
}
