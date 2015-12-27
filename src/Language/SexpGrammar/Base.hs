{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.SexpGrammar.Base where

import Prelude hiding ((.), id)

import Control.Category
import Control.Monad.Except
import Control.Monad.State

import Data.Functor.Foldable (Fix (..))
import Data.StackPrism.Extra
import Data.StackPrism.Generic
import Text.Printf

import Data.InvertibleGrammar
import Language.Sexp.Types

data SexpGrammar a b where
  -- | Dispatch single Sexp, which must match literal atom
  GAtom :: Atom -> SexpGrammar (Sexp :- t) t

  -- | Transform Sexp into t'
  GList :: Grammar ListGrammar t t' -> SexpGrammar (Sexp :- t) t'

instance
  ( MonadPlus m
  , MonadError String m
  ) => InvertibleGrammar m SexpGrammar where
  parseWithGrammar (GAtom a) (s :- t) =
    case s of
      Fix (Atom a') | a == a' -> return t
      _ -> throwError $ "Expected literal atom " ++ show a ++ " but found " ++ show s
  parseWithGrammar (GList g) (s :- t) = do
    case s of
      Fix (List xs) -> evalStateT (parseWithGrammar g t) (ListCtx xs)
      _ -> throwError $ "Expected list but found: " ++ show s



data ListGrammar a b where
  -- | Dispatch single list element with a grammar
  GElem :: String
        -- ^ Rule name
        -> Grammar SexpGrammar (Sexp :- t) t'
        -- ^ Grammar to parse list element at current position with
        -> ListGrammar t t'

newtype ListCtx = ListCtx { getItems :: [Sexp] }

instance
  ( MonadPlus m
  , MonadState ListCtx m
  , MonadError String m
  ) => InvertibleGrammar m ListGrammar where
  parseWithGrammar (GElem ruleName gram) t = do
    xs <- gets getItems
    case xs of
      []      ->
        throwError $
        printf "Failed to parse \"%s\": expected one more list element" ruleName
      x : xs' -> do
        modify $ \s -> s { getItems = xs' }
        parseWithGrammar gram (x :- t)


multiple :: (forall u. Grammar g u (a :- u)) -> Grammar g t ([a] :- t)
multiple gram = GenPrism nil
            >>> Many (gram >>> GenPrism cons)
            >>> GenPrism (inStack rev)
  where
    nil  :: StackPrism u ([a] :- u)
    cons :: StackPrism (a :- [a] :- u) ([a] :- u)
    PrismList (P nil :& P cons) = mkPrismList :: StackPrisms [b]
    rev :: StackPrism [a] [a]
    rev = stackPrism reverse (Just . reverse)

parse
  :: (MonadPlus m, MonadError String m, InvertibleGrammar m g)
  => Grammar g (Sexp :- ()) (a :- ())
  -> Sexp
  -> m a
parse gram input =
  (\(x :- _) -> x) <$> parseWithGrammar gram (input :- ())
