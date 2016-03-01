{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-tabs               #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}

module Language.Sexp.Lexer
  ( lexSexp
  ) where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B8
import Language.Sexp.Token
import Language.Sexp.Types (Position (..))
}

%wrapper "posn-bytestring"

$whitechar   = [\ \t\n\r\f\v]

$digit       = 0-9
$hex         = [0-9 A-F a-f]
$alpha       = [a-z A-Z]

$graphic     = [$alpha $digit \!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~ \(\)\,\;\[\]\`\{\} \:\"\'\_]

@intnum      = [\-\+]? $digit+
@scinum      = [\-\+]? $digit+ ([\.]$digit+)? ([eE] [\-\+]? $digit+)?

$charesc     = [abfnrtv\\\"]
@escape      = \\ ($charesc | $digit+ | x $hex+)
@string      = $graphic # [\"\\] | " " | @escape

$idinitial   = [$alpha \!\$\%\&\*\/\<\=\>\?\~\_\^\.\+\-]
$idsubseq    = [$idinitial $digit \:]
@identifier  = $idinitial $idsubseq*
@keyword     = ":" $idsubseq+

:-

$whitechar+        ;
";".*              ;
"("                { just TokLParen       }
")"                { just TokRParen       }
"["                { just TokLBracket     }
"]"                { just TokRBracket     }
"'" / $graphic     { just TokQuote        }
"#t"               { just (TokBool True)  }
"#f"               { just (TokBool False) }
"#" / $graphic     { just TokHash         }
@intnum            { TokInt     `via` readInteger       }
@scinum            { TokReal    `via` (read . T.unpack) }
@identifier        { TokSymbol  `via` id                }
@keyword           { TokKeyword `via` id                }
\" @string* \"     { TokStr     `via` readString        }
.                  { TokUnknown `via` T.head            }

{

readInteger :: T.Text -> Integer
readInteger str =
  case signed decimal str of
    Left err -> error $ "Lexer is broken: " ++ err
    Right (a, rest)
      | T.null (T.strip rest) -> a
      | otherwise -> error $ "Lexer is broken, leftover: " ++ show rest

readString :: T.Text -> T.Text
readString =
  T.pack . read . T.unpack

just :: Token -> AlexPosn -> B8.ByteString -> LocatedBy AlexPosn Token
just tok pos _ =
  L pos tok

via :: (a -> Token) -> (T.Text -> a) -> AlexPosn -> B8.ByteString -> LocatedBy AlexPosn Token
via ftok f pos str =
  L pos . ftok . f  . TL.toStrict . decodeUtf8 $ str

lexSexp :: FilePath -> B8.ByteString -> [LocatedBy Position Token]
lexSexp f = map (mapPosition fixPos) . alexScanTokens
  where
    fixPos (AlexPn _ l c) = Position l c

}
