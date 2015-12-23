{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
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
import qualified Data.List.NonEmpty as NE
import Text.Printf

import Data.InvertibleGrammar
import Data.PartialIso

import Language.Sexp.Types

data ListCtx = ListCtx
  { lcItems :: [Sexp]
  , lcEnd   :: Sexp
  }

nilCtx :: ListCtx
nilCtx = ListCtx [] (Fix Nil)

mkListCtx :: (MonadError String m) => Bool -> Sexp -> m ListCtx
mkListCtx _          (Fix Nil) = return nilCtx
mkListCtx properList (Fix (List xs end))
  | properList && end == Fix Nil || not properList
  = return $ ListCtx (NE.toList xs) end
mkListCtx properList x =
  throwError $ printf "Expected %s list but found %s" expected (show x)
  where
    expected | properList = "proper nil-terminated"
             | otherwise  = "any"
-- mkListCtx True  (List xs end@(Fix Nil)) = return $ ListCtx (NE.toList xs) end
-- mkListCtx False (List xs end)           = return $ ListCtx (NE.toList xs) end
-- mkListCtx True  x                       =
--   throwError $ "Expected proper nil-terminated list but found " ++ show x
-- mkListCtx False x                       =
--   throwError $ "Expected any list but found " ++ show x


data SexpGrammar a b where
  -- Dispatch single Sexp, which must match literal atom
  LitAtom     :: Atom -> SexpGrammar (Sexp :- t) t
  -- Transform Sexp into t'
  DescendList :: Grammar ListGrammar t t' -> SexpGrammar (Sexp :- t) t'

data ListGrammar a b where
  -- Dispatch single sexp with grammar
  Head :: String -- ^ Rule name.
       -- | Grammar to parse list element at current position with.
       -> Grammar SexpGrammar (Sexp :- t) t'
       -> ListGrammar t t'

instance
  ( MonadPlus m
  , MonadError String m
  ) => InvertibleGrammar m SexpGrammar where
  parseWithGrammar (LitAtom a) (s :- t) =
    case s of
      Fix (Atom a') | a == a' -> do
        return t
      _ -> throwError $ "Expected literal atom " ++ show a ++ " but found " ++ show s
  parseWithGrammar (DescendList g) (s :- t) = do
    ctx <- mkListCtx True s
    evalStateT (parseWithGrammar g t) ctx


instance
  ( MonadPlus m
  , MonadState ListCtx m
  , MonadError String m
  ) => InvertibleGrammar m ListGrammar where
  parseWithGrammar (Head ruleName gram) t = do
    xs <- gets lcItems
    case xs of
      []    ->
        throwError $
        printf "Failed to parse \"%s\": expected one more list element" ruleName
      x:xs' -> do
        modify $ \s -> s { lcItems = xs' }
        parseWithGrammar gram (x :- t)

class FromSexp a where
  -- Convert Sexp into a
  sexpGrammar :: Grammar SexpGrammar (Sexp :- t) (a :- t)

instance (FromSexp a) => FromSexp [a] where
  sexpGrammar :: Grammar SexpGrammar (Sexp :- t) ([a] :- t)
  sexpGrammar =
    Gram $ DescendList $
    multiple $ Gram $ Head "sexpGrammar for [a]" sexpGrammar

multiple :: (forall u. Grammar g u (a :- u)) -> Grammar g t ([a] :- t)
multiple gram = isoToGrammar isoNil >>> Many (gram >>> isoToGrammar isoCons)
