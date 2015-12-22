{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.SexpGrammar.Base where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Foldable (Fix (..))
-- import qualified Data.List.NonEmpty as NE

import Data.Invertible
import Data.InvertibleGrammar
import Data.PartialIso

import Language.Sexp.Types

data ListCtx = ListCtx
  { lcItems :: [Sexp]
  , lcEnd   :: Sexp
  }

data SexpGrammar m a b where
  -- Dispatch single Sexp, which must match literal atom
  LitAtom     :: Atom -> SexpGrammar m (Sexp :- t) t
  -- Transform Sexp into t'
  DescendList :: Grammar (StateT ListCtx m) (ListGrammar (StateT ListCtx m)) t t' -> SexpGrammar m (Sexp :- t) t'

data ListGrammar m a b where
  -- Dispatch single sexp with grammar
  Head :: Grammar m (SexpGrammar m) (Sexp :- t) t' -> ListGrammar m t t'

instance (MonadError String m) => Invertible (SexpGrammar m) where
  type InvertibleCtx (SexpGrammar m) = m
  applyForward (LitAtom a) (s :- t) =
    case s of
      Fix (Atom a') | a == a' -> do
        return t
      _ -> throwError $ "Expected literal atom " ++ show a ++ " but found " ++ show s
  applyForward (DescendList _) _ = undefined
  -- applyForward (DescendList g) (s :- t) =
  --   case s of
  --     Fix (List xs end) -> do undefined
  --       -- applyForward g t
  --     _ -> throwError $ "Expected list but found " ++ show s
  applyBackward = undefined


class FromSexp m a where
  -- Convert Sexp into a
  sexpGrammar :: Grammar m (SexpGrammar m) (Sexp :- t) (a :- t)

instance (FromSexp (StateT ListCtx m) a, MonadError String m) => FromSexp m [a] where
  sexpGrammar :: Grammar m (SexpGrammar m) (Sexp :- t) ([a] :- t)
  sexpGrammar = Gram $ DescendList $
    isoToGrammar isoNil >>> Many (Gram (Head (sexpGrammar >>> isoToGrammar isoCons)))

