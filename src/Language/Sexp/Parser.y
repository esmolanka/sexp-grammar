{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# OPTIONS_GHC -fno-warn-tabs                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches      #-}

module Language.Sexp.Parser (parseSexp, parseProgram) where

import Data.List
import Data.Text (Text)
import qualified Data.Scientific
import qualified Data.Text as T

import Language.Sexp.Lexer
import Language.Sexp.Types

}

%name parseSexp Sexp
%name parseProgram Program
%error { parseError }
%tokentype { LocatedBy Position Token }
%monad { Either String }

%token
  '('            { L _ TokLParen   }
  ')'            { L _ TokRParen   }
  '['            { L _ TokLBracket }
  ']'            { L _ TokRBracket }
  '.'            { L _ TokDot      }
  "'"            { L _ TokQuote    }
  '#'            { L _ TokHash     }
  Symbol         { L _ (TokSymbol _) }
  Keyword        { L _ (TokKeyword _) }
  Integer        { L _ (TokInt _) }
  Real           { L _ (TokReal _) }
  String         { L _ (TokStr _) }
  Bool           { L _ (TokBool _) }

%%

Program :: { [Sexp] }
  : list(Sexp)   { $1 }

Sexp :: { Sexp }
  : Atom                             { Fix $ Atom   $1 }
  | bracketed('(', List, ')')        { Fix $ List   $1 }
  | bracketed('[', Vector, ']')      { Fix $ Vector $1 }
  | '#' bracketed('(', Vector, ')')  { Fix $ Vector $2 }
  | "'" Sexp                         { Fix $ List (Fix (Atom (AtomSymbol "quote")) : asList $2) }

Atom :: { Atom }
  : Bool         { AtomBool (getBool (extract $1))    }
  | Integer      { AtomInt  (getInt  (extract $1)) }
  | Real         { AtomReal (getReal (extract $1)) }
  | String       { AtomString (getString (extract $1)) }
  | Symbol       { AtomSymbol (getSymbol (extract $1)) }
  | Keyword      { AtomKeyword (getKeyword (extract $1)) }

List :: { [Sexp] }
  : list(Sexp) '.' Sexp   { $1 ++ [$3] }
  | list(Sexp)            { $1 ++ [Fix Nil] }

Vector :: { [Sexp] }
  : list(Sexp)        { $1 }

bracketed(o, p, c)
  : o p c          { $2 }

list(p)
  : {- empty -}    { [] }
  | p list(p)      { $1 : $2 }

{

asList :: Sexp -> [Sexp]
asList a@(Fix (Atom _))  = [a, Fix Nil]
asList (Fix (List ls))   = ls
asList (Fix (Vector ls)) = ls ++ [Fix Nil]

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  []    -> Left "Unexpected EOF"
  t : _ -> Left $ "Unexpected token: " ++ show t

}
