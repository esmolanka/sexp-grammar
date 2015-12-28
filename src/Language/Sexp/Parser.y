{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# OPTIONS_GHC -fno-warn-tabs                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches      #-}

module Language.Sexp.Parser
  ( parseSexps
  , parseSexp
  ) where

import Data.Functor.Foldable (Fix (..))
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy

import Text.PrettyPrint.Leijen.Text

import Language.Sexp.Token
import Language.Sexp.Lexer
import Language.Sexp.Types
}

%name parseSexp_ Sexp
%name parseSexps_ Sexps
%error { parseError }
%tokentype { LocatedBy Position Token }
%monad { Either String }

%token
  '('            { L _ TokLParen      }
  ')'            { L _ TokRParen      }
  '['            { L _ TokLBracket    }
  ']'            { L _ TokRBracket    }
  "'"            { L _ TokQuote       }
  '#'            { L _ TokHash        }
  Symbol         { L _ (TokSymbol  _) }
  Keyword        { L _ (TokKeyword _) }
  Integer        { L _ (TokInt     _) }
  Real           { L _ (TokReal    _) }
  String         { L _ (TokStr     _) }
  Bool           { L _ (TokBool    _) }

%%

Sexps :: { [Sexp] }
  : list(Sexp)   { $1 }

Sexp :: { Sexp }
  : Atom                                  { Fix $ Atom $1 }
  | bracketed('(', ListBody, ')')         { $1 }
  | bracketed('[', VectorBody, ']')       { $1 }
  | '#' bracketed('(', VectorBody, ')')   { $2 }
  | "'" Sexp                              { Fix $ Quoted $2 }

Atom :: { Atom }
  : Bool         { AtomBool    (getBool    (extract $1)) }
  | Integer      { AtomInt     (getInt     (extract $1)) }
  | Real         { AtomReal    (getReal    (extract $1)) }
  | String       { AtomString  (getString  (extract $1)) }
  | Symbol       { AtomSymbol  (getSymbol  (extract $1)) }
  | Keyword      { AtomKeyword (mkKw (getKeyword (extract $1))) }

ListBody :: { Sexp }
  : list(Sexp)   { Fix $ List $1 }

VectorBody :: { Sexp }
  : list(Sexp)   { Fix $ Vector $1 }


-- Utils

bracketed(o, p, c)
  : o p c                  { $2 }

rev_list1(p)
  : p                      { [$1]    }
  | rev_list1(p) p         { $2 : $1 }

list1(p)
  : rev_list1(p)           { reverse $1 }

list(p)
  : {- empty -}            { [] }
  | list1(p)               { $1 }

{
mkKw :: Text -> Kw
mkKw t = case T.uncons t of
  Nothing -> error "Keyword should start with :"
  Just (_, rs) -> Kw rs

parseSexp :: FilePath -> String -> Either String Sexp
parseSexp fn inp =
  case parseSexp_ (lexSexp fn inp) of
    Left err -> Left $ fn ++ ":" ++ err
    Right a  -> Right a

parseSexps :: FilePath -> String -> Either String [Sexp]
parseSexps fn inp =
  case parseSexps_ (lexSexp fn inp) of
    Left err -> Left $ fn ++ ":" ++ err
    Right a  -> Right a

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "EOF: Unexpected end of file"
  (L pos tok : _) ->
    Left $ Lazy.unpack . displayT . renderPretty 0.8 80 $
      pretty pos <> colon <+> "Unexpected token:" <+> pretty tok
}
