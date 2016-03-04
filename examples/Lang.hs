{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lang where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Reader
import Data.Data (Data)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Data.Foldable (foldl)
#endif
import Data.Maybe

import Language.SexpGrammar
import Language.SexpGrammar.Generic
import GHC.Generics
import Data.Coerce

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f

fx :: Grammar g (f (Fix f) :- t) (Fix f :- t)
fx = iso coerce coerce

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

data Literal
  = LitInt Int
  | LitDouble Double
    deriving (Eq, Show, Generic)

asInt :: Literal -> Maybe Int
asInt (LitDouble _) = Nothing
asInt (LitInt a) = Just a

asDouble :: Literal -> Double
asDouble (LitDouble a) = a
asDouble (LitInt a) = fromIntegral a

instance SexpIso Literal where
  sexpIso = match
    $ With (\i -> i . int)
    $ With (\d -> d . double)
    $ End

newtype Ident = Ident String
    deriving (Eq, Ord, Show, Generic)

instance SexpIso Ident where
  sexpIso = with symbol'

data Func
  = Prim Prim
  | Named Ident
    deriving (Eq, Show, Generic)

instance SexpIso Func where
  sexpIso = match
    $ With (\prim -> prim . sexpIso)
    $ With (\named -> named . sexpIso)
    $ End

data Prim
  = Add
  | Mul
  | Sub
  | Div
    deriving (Eq, Show, Bounded, Enum, Data, Generic)

instance SexpIso Prim where
  sexpIso = match
    $ With (\_Add -> _Add . sym "+")
    $ With (\_Mul -> _Mul . sym "*")
    $ With (\_Sub -> _Sub . sym "-")
    $ With (\_Div -> _Div . sym "/")
    $ End

evalP :: Prim -> [Literal] -> Literal
evalP p =
  case p of
    Add -> \ls -> fromMaybe (LitDouble $ sum $ map asDouble ls)
                            (LitInt . sum <$> traverse asInt ls)
    Mul -> \ls -> fromMaybe (LitDouble $ product $ map asDouble ls)
                            (LitInt . product <$> traverse asInt ls)
    Sub -> \[a,b] -> fromMaybe (LitDouble $ asDouble a - asDouble b)
                               ((LitInt .) . (-) <$> asInt a <*> asInt b)
    Div -> \[a,b] -> fromMaybe (LitDouble $ asDouble a / asDouble b)
                               ((LitInt .) . div <$> asInt a <*> asInt b)

type Expr = Fix ExprF

data ExprF e
  = Lit Literal
  | Var Ident
  | Let Ident e e
  | Apply Prim [e]
  | Cond e e e
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

exprIso :: SexpG (ExprF (Fix ExprF))
exprIso = match
  $ With (\_Lit -> _Lit . sexpIso)
  $ With (\_Var -> _Var . sexpIso)
  $ With (\_Let -> _Let . list
            ( el (sym "let")    >>>
              el sexpIso        >>>
              el (fx . exprIso) >>>
              el (fx . exprIso) ) )
  $ With (\_Apply -> _Apply . list
            ( el sexpIso        >>>
              rest (fx . exprIso ) ) )
  $ With (\_Cond -> _Cond . list
            ( el (sym "if")     >>>
              el (fx . exprIso) >>>
              el (fx . exprIso) >>>
              el (fx . exprIso) ) )
  $ End

instance SexpIso (Fix ExprF) where
  sexpIso = fx . exprIso

type PEvalM = Reader (M.Map Ident Literal)

partialEval :: Expr -> Expr
partialEval e = runReader (cata alg e) M.empty
  where
    alg :: ExprF (PEvalM Expr) -> PEvalM Expr
    alg (Lit a) = return (Fix $ Lit a)
    alg (Var v) = do
      val <- asks (M.lookup v)
      case val of
        Nothing -> return $ Fix (Var v)
        Just a  -> return $ Fix (Lit a)
    alg (Let n e r) = do
      e' <- e
      r' <- case unFix e' of
              Lit a -> local (M.insert n a) r
              _ -> r
      case unFix r' of
        Lit a -> return (Fix $ Lit a)
        _ -> case M.findWithDefault 0 n (gatherFreeVars r') of
               0 -> return r'
               1 -> return $ inline (M.singleton n e') r'
               _ -> return (Fix $ Let n e' r')
    alg (Apply p args) = do
      args' <- sequence args
      let args'' = getLits args'
      return $ Fix $ maybe (Apply p args') (Lit . evalP p) args''
    alg (Cond c t f) = do
      c' <- c
      t' <- t
      f' <- f
      case c' of
        Fix (Lit (LitInt 0)) -> return f'
        Fix (Lit (LitDouble 0.0)) -> return f'
        Fix (Lit _) -> return t'
        _ -> return $ Fix $ Cond c' t' f'

type FreeVarsM = Reader (S.Set Ident)

gatherFreeVars :: Expr -> M.Map Ident Int
gatherFreeVars e = runReader (cata alg e) S.empty
  where
    alg :: ExprF (FreeVarsM (M.Map Ident Int)) -> FreeVarsM (M.Map Ident Int)
    alg (Let n e r) = do
      e' <- e
      r' <- local (S.insert n) r
      return $ e' <> r'
    alg (Var n) = do
      bound <- asks (S.member n)
      return $ if bound then M.empty else M.singleton n 1
    alg other = foldl (M.unionWith (+)) M.empty <$> sequence other

getLits :: [Expr] -> Maybe [Literal]
getLits = sequence . map getLit
  where
    getLit (Fix (Lit a)) = Just a
    getLit _ = Nothing

type InlineM = Reader (M.Map Ident Expr)

inline :: M.Map Ident Expr -> Expr -> Expr
inline env e = runReader (cata alg e) env
  where
    alg :: ExprF (InlineM Expr) -> InlineM Expr
    alg (Var n) = do
      subst <- asks (M.lookup n)
      case subst of
        Nothing -> return $ Fix $ Var n
        Just e  -> return e
    alg (Let n e r) = do
      e' <- e
      r' <- local (M.delete n) r
      return $ Fix $ Let n e' r'
    alg other = Fix <$> sequence other

test :: String -> String
test str = either error id $ do
  e <- decode (B8.pack str)
  either error (return . B8.unpack) (encodePretty (partialEval e))

-- λ> test "(let foo (/ 42 2) (let bar (* foo 1.5 baz) (if 0 foo (+ 1 bar))))"
-- "(+ 1 (* 21 1.5 baz))"
