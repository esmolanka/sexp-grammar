{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# OPTIONS_GHC -fno-warn-tabs                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds        #-}
{-# OPTIONS_GHC -fno-warn-unused-matches      #-}

module Language.Sexp.Parser
  ( parseSexp_
  , parseSexps_
  ) where

import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B8

import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.ShowS as Render

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
  '{'            { L _ TokLBrace      }
  '}'            { L _ TokRBrace      }
  "'"            { L _ TokQuote       }
  Symbol         { L _ (TokSymbol  _) }
  Keyword        { L _ (TokKeyword _) }
  Integer        { L _ (TokInt     _) }
  Real           { L _ (TokReal    _) }
  String         { L _ (TokStr     _) }

%%

Sexps :: { [Sexp] }
  : list(Sexp)                            { $1 }

Sexp :: { Sexp }
  : Atom                                  { (\a p -> Atom p a) @@ $1 }
  | '(' list(Sexp) ')'                    { const (\p -> List p $2) @@ $1 }
  | '[' list(Sexp) ']'                    { const (\p -> Vector p $2) @@ $1 }
  | '{' list(Sexp) '}'                    { const (\p -> BraceList p $2) @@ $1 }
  | "'" Sexp                              { const (\p -> Quoted p $2) @@ $1 }

Atom :: { LocatedBy Position Atom }
  : Integer                               { fmap (AtomInt     . getInt)          $1 }
  | Real                                  { fmap (AtomReal    . getReal)         $1 }
  | String                                { fmap (AtomString  . getString)       $1 }
  | Symbol                                { fmap (AtomSymbol  . getSymbol)       $1 }
  | Keyword                               { fmap (AtomKeyword . Kw . getKeyword) $1 }

-- Utils

rev_list1(p)
  : p                      { [$1]    }
  | rev_list1(p) p         { $2 : $1 }

list1(p)
  : rev_list1(p)           { reverse $1 }

list(p)
  : {- empty -}            { [] }
  | list1(p)               { $1 }

{

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "EOF: Unexpected end of file"
  (L pos tok : _) ->
    Left $ flip Render.renderShowS [] . layoutPretty (LayoutOptions (AvailablePerLine 80 0.8)) $
      pretty pos <> colon <+> "Unexpected token:" <+> pretty tok
}
