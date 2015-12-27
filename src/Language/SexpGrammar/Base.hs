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

import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy

import Data.Functor.Foldable (Fix (..))
import Data.StackPrism.Extra
import Data.StackPrism.Generic

import Data.InvertibleGrammar
import Language.Sexp

data SexpGrammar a b where
  GAtom :: Grammar AtomGrammar (Atom :- t) t' -> SexpGrammar (Sexp :- t) t'
  GList :: Grammar ListGrammar t           t' -> SexpGrammar (Sexp :- t) t'

instance
  ( MonadPlus m
  , MonadError String m
  ) => InvertibleGrammar m SexpGrammar where

  parseWithGrammar (GAtom g) (s :- t) =
    case s of
      Fix (Atom a) -> parseWithGrammar g (a :- t)
      _ ->
        throwError $ "Expected atom but found: " ++ Lazy.unpack (printSexp s)

  parseWithGrammar (GList g) (s :- t) = do
    case s of
      Fix (List xs) -> evalStateT (parseWithGrammar g t) (ListCtx xs)
      _ ->
        throwError $ "Expected list but found: " ++ Lazy.unpack (printSexp s)

  genWithGrammar (GAtom g) t = do
    (a :- t') <- genWithGrammar g t
    return (Fix (Atom a) :- t')
  genWithGrammar (GList g) t = do
    (t', ListCtx xs) <- runStateT (genWithGrammar g t) (ListCtx [])
    return (Fix (List xs) :- t')

data AtomGrammar a b where
  GSym     :: Text -> AtomGrammar (Atom :- t) t
  GKw      :: Text -> AtomGrammar (Atom :- t) t
  GBool    :: AtomGrammar (Atom :- t) (Bool :- t)
  GInt     :: AtomGrammar (Atom :- t) (Integer :- t)
  GReal    :: AtomGrammar (Atom :- t) (Scientific :- t)
  GString  :: AtomGrammar (Atom :- t) (Text :- t)
  GSymbol  :: AtomGrammar (Atom :- t) (Text :- t)
  GKeyword :: AtomGrammar (Atom :- t) (Text :- t)

instance
  ( MonadPlus m
  , MonadError String m
  ) => InvertibleGrammar m AtomGrammar where

  parseWithGrammar (GSym sym') (atom :- t) =
    case atom of
      AtomSymbol sym | sym' == sym -> return t
      other -> throwError $ "Expected symbol " ++ show sym' ++ ", got " ++ show other

  parseWithGrammar (GKw kw') (atom :- t) =
    case atom of
      AtomKeyword kw | kw' == kw -> return t
      other -> throwError $ "Expected keyword " ++ show kw' ++ ", got " ++ show other

  parseWithGrammar GBool (atom :- t) =
    case atom of
      AtomBool a -> return $ a :- t
      _          -> throwError "Expected bool, got something else"

  parseWithGrammar GInt (atom :- t) =
    case atom of
      AtomInt a -> return $ a :- t
      _         -> throwError "Expected int, got something else"

  parseWithGrammar GReal (atom :- t) =
    case atom of
      AtomReal a -> return $ a :- t
      _          -> throwError "Expected real, got something else"

  parseWithGrammar GString (atom :- t) =
    case atom of
      AtomString a -> return $ a :- t
      _            -> throwError "Expected string, got something else"

  parseWithGrammar GSymbol (atom :- t) =
    case atom of
      AtomSymbol a -> return $ a :- t
      _            -> throwError "Expected symbol, got something else"

  parseWithGrammar GKeyword (atom :- t) =
    case atom of
      AtomKeyword a -> return $ a :- t
      _             -> throwError "Expected keyword, got something else"

  genWithGrammar (GSym sym) t = return (AtomKeyword sym :- t)
  genWithGrammar (GKw kw) t = return (AtomKeyword kw :- t)
  genWithGrammar GBool (a :- t) = return (AtomBool a :- t)
  genWithGrammar GInt (a :- t) = return (AtomInt a :- t)
  genWithGrammar GReal (a :- t) = return (AtomReal a :- t)
  genWithGrammar GString (a :- t) = return (AtomString a :- t)
  genWithGrammar GSymbol (a :- t) = return (AtomSymbol a :- t)
  genWithGrammar GKeyword (a :- t) = return (AtomKeyword a :- t)


data ListGrammar a b where
  -- | Dispatch single list element with a grammar
  GElem :: Grammar SexpGrammar (Sexp :- t) t'
        -- ^ Grammar to parse list element at current position with
        -> ListGrammar t t'

newtype ListCtx = ListCtx { getItems :: [Sexp] }

instance
  ( MonadPlus m
  , MonadState ListCtx m
  , MonadError String m
  ) => InvertibleGrammar m ListGrammar where
  parseWithGrammar (GElem g) t = do
    xs <- gets getItems
    case xs of
      [] -> throwError $ "Unexpected end of list"
      x:xs' -> do
        modify $ \s -> s { getItems = xs' }
        parseWithGrammar g (x :- t)

  genWithGrammar (GElem g) t = do
    (x :- t') <- genWithGrammar g t
    modify $ \s -> s { getItems = x : getItems s }
    return t'

multiple :: (forall u. Grammar g u (a :- u)) -> Grammar g t ([a] :- t)
multiple gram = GenPrism nil
            >>> Many (gram >>> GenPrism cons)
            >>> embedPrism rev
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


gen
  :: (MonadPlus m, MonadError String m, InvertibleGrammar m g)
  => Grammar g (Sexp :- ()) (a :- ())
  -> a
  -> m Sexp
gen gram input =
  (\(x :- _) -> x) <$> genWithGrammar gram (input :- ())
