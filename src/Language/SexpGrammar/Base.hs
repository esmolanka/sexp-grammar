{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.SexpGrammar.Base where

import Control.Category
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Foldable (Fix (..))
import Data.StackPrism
import Data.StackPrism.Generic
import Prelude hiding ((.), id)
import Text.Printf

import Data.InvertibleGrammar

import Language.Sexp.Types

data SexpGrammar a b where
  -- | Dispatch single Sexp, which must match literal atom
  LitAtom     :: Atom -> SexpGrammar (Sexp :- t) t

  -- | Transform Sexp into t'
  DescendList :: Grammar ListGrammar t t' -> SexpGrammar (Sexp :- t) t'

data ListGrammar a b where
  -- | Dispatch single list element with a grammar
  Head :: String
       -- ^ Rule name
       -> Grammar SexpGrammar (Sexp :- t) t'
       -- ^ Grammar to parse list element at current position with
       -> ListGrammar t t'

data ListCtx = ListCtx { getItems :: [Sexp] }

nilCtx :: ListCtx
nilCtx = ListCtx []

mkListCtx :: (MonadError String m) => Sexp -> m ListCtx
mkListCtx (Fix (List xs)) = return $ ListCtx xs
mkListCtx x = throwError $ "Expected list but found: " ++ show x


instance
  ( MonadPlus m
  , MonadError String m
  ) => InvertibleGrammar m SexpGrammar where
  parseWithGrammar (LitAtom a) (s :- t) =
    case s of
      Fix (Atom a') | a == a' -> return t
      _ -> throwError $ "Expected literal atom " ++ show a ++ " but found " ++ show s
  parseWithGrammar (DescendList g) (s :- t) = do
    ctx <- mkListCtx s
    evalStateT (parseWithGrammar g t) ctx

instance
  ( MonadPlus m
  , MonadState ListCtx m
  , MonadError String m
  ) => InvertibleGrammar m ListGrammar where
  parseWithGrammar (Head ruleName gram) t = do
    xs <- gets getItems
    case xs of
      []      ->
        throwError $
        printf "Failed to parse \"%s\": expected one more list element" ruleName
      x : xs' -> do
        modify $ \s -> s { getItems = xs' }
        parseWithGrammar gram (x :- t)

class FromSexp a where
  -- | Convert Sexp into a
  sexpGrammar :: Grammar SexpGrammar (Sexp :- t) (a :- t)

stackPrismFirst :: StackPrism a b -> StackPrism (a :- t) (b :- t)
stackPrismFirst prism = stackPrism f g
  where
    f (a :- t) = forward prism a :- t
    g (b :- t) = (:- t) <$> backward prism b

instance FromSexp Bool where
  sexpGrammar = fromRevStackPrism $ stackPrismFirst p
    where
      p :: StackPrism Bool Sexp
      p = stackPrism (Fix . Atom . AtomBool) g
      g (Fix (Atom (AtomBool b))) = Just b
      g _                         = Nothing

instance (FromSexp a) => FromSexp [a] where
  sexpGrammar :: Grammar SexpGrammar (Sexp :- t) ([a] :- t)
  sexpGrammar =
    Gram $ DescendList $
    multiple $ Gram $ Head "sexpGrammar for [a]" sexpGrammar

multiple :: (forall u. Grammar g u (a :- u)) -> Grammar g t ([a] :- t)
multiple gram =
      fromStackPrism nil
  >>> Many (gram >>> fromStackPrism cons)
  >>> fromStackPrism (stackPrismFirst rev)
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
