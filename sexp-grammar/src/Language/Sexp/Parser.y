{
{-# LANGUAGE CPP               #-}
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

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

#if MIN_VERSION_prettyprinter(1,7,0)
import qualified Data.Text.Prettyprint.Doc.Render.String as Render
#else
import qualified Data.Text.Prettyprint.Doc.Render.ShowS as Render
#endif

import Language.Sexp.Token
import Language.Sexp.Lexer
import Language.Sexp.Types
}

%name parseSexp_ Sexp_
%name parseSexps_ Sexps_
%error { parseError }
%tokentype { LocatedBy Position Token }
%monad { Either String }

%token
  '('            { _ :< TokLParen     }
  ')'            { _ :< TokRParen     }
  '['            { _ :< TokLBracket   }
  ']'            { _ :< TokRBracket   }
  '{'            { _ :< TokLBrace     }
  '}'            { _ :< TokRBrace     }
  PREFIX         { _ :< (TokPrefix _) }
  SYMBOL         { _ :< (TokSymbol _) }
  NUMBER         { _ :< (TokNumber _) }
  STRING         { _ :< (TokString _) }

  EOF            { _ :< TokEOF        }

%%

Sexps_ :: { [Sexp] }
  : Sexps EOF                             { $1 }

Sexps :: { [Sexp] }
  : list(Sexp)                            { $1 }

Sexp_ :: { Sexp }
  : Sexp EOF                              { $1 }

Sexp :: { Sexp }
  : Atom                                  { AtomF                       @@ $1 }
  | '(' list(Sexp) ')'                    { const (ParenListF $2)       @@ $1 }
  | '[' list(Sexp) ']'                    { const (BracketListF $2)     @@ $1 }
  | '{' list(Sexp) '}'                    { const (BraceListF $2)       @@ $1 }
  | PREFIX Sexp                           { const (ModifiedF
                                                    (getPrefix (extract $1))
                                                    $2)                 @@ $1 }

Atom :: { LocatedBy Position Atom }
  : NUMBER                                { fmap (AtomNumber . getNumber) $1 }
  | STRING                                { fmap (AtomString . getString) $1 }
  | SYMBOL                                { fmap (AtomSymbol . getSymbol) $1 }

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

type Sexp = Fix (Compose (LocatedBy Position) SexpF)

(@@) :: (a -> e (Fix (Compose (LocatedBy p) e))) -> LocatedBy p a -> Fix (Compose (LocatedBy p) e)
(@@) f (p :< a) = Fix . Compose . (p :<) . f $ a

parseError :: [LocatedBy Position Token] -> Either String b
parseError toks = case toks of
  [] ->
    Left "EOF: Unexpected end of file"
  (pos :< tok : _) ->
    Left $ flip Render.renderShowS [] . layoutPretty (LayoutOptions (AvailablePerLine 80 0.8)) $
      pretty pos <> colon <+> "Unexpected token:" <+> pretty tok
}
