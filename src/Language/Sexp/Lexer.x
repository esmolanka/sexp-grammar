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
import Language.Sexp.Token
}

%wrapper "posn"

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
"("                { just TokLParen                }
")"                { just TokRParen                }
"["                { just TokLBracket              }
"]"                { just TokRBracket              }
"'" / $graphic     { just TokQuote                 }
"#t"               { just (TokBool True)           }
"#f"               { just (TokBool False)          }
"#" / $graphic     { just TokHash                  }
@identifier        { TokSymbol  `via` T.pack       }
@keyword           { TokKeyword `via` T.pack       }
@intnum            { TokInt     `via` read         }
@scinum            { TokReal    `via` read         }
\" @string* \"     { TokStr `via` (T.pack . read)  }
.                  { TokUnknown `via` head         }

{

just :: Token -> AlexPosn -> String -> LocatedBy AlexPosn Token
just tok pos _ = L pos tok

via :: (a -> Token) -> (String -> a) -> AlexPosn -> String -> LocatedBy AlexPosn Token
via ftok f pos str = L pos (ftok (f str))

lexSexp :: FilePath -> String -> [LocatedBy Position Token]
lexSexp f = map (mapPosition fixPos) . alexScanTokens
  where
    fixPos (AlexPn _ l c) = Position l c

}
