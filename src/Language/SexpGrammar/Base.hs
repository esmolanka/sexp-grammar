{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.SexpGrammar.Base
  ( SexpGrammar (..)
  , SeqGrammar (..)
  , AtomGrammar (..)
  , PropGrammar (..)
  , parse
  , gen
  , SexpG
  , SexpG_
  , module Data.InvertibleGrammar
  ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
#if MIN_VERSION_mtl(2, 2, 0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Control.Monad.State

import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Map as M
import Data.Map (Map)
import Data.StackPrism

import Data.InvertibleGrammar
import Language.Sexp

type SexpG a = forall t. Grammar SexpGrammar (Sexp :- t) (a :- t)
type SexpG_ = forall t. Grammar SexpGrammar (Sexp :- t) t

data SexpGrammar a b where
  GAtom :: Grammar AtomGrammar (Atom :- t) t' -> SexpGrammar (Sexp :- t) t'
  GList :: Grammar SeqGrammar t            t' -> SexpGrammar (Sexp :- t) t'
  GVect :: Grammar SeqGrammar t            t' -> SexpGrammar (Sexp :- t) t'

parseSeq :: (MonadError String m, InvertibleGrammar (StateT SeqCtx m) g) => [Sexp] -> g a b -> a -> m b
parseSeq xs g t = do
  (a, SeqCtx rest) <- runStateT (parseWithGrammar g t) (SeqCtx xs)
  unless (null rest) $
    throwError $ "Unexpected leftover elements: " ++ (unwords $ map (Lazy.unpack . printSexp) rest)
  return a

posError :: (MonadError String m) => Sexp -> String -> m a
posError sexp str =
  let Position line col = getPos sexp
  in  throwError $ concat
        [ show line, ":", show col, ": expected "
        , str, ", but got: ", Lazy.unpack (printSexp sexp)
        ]

instance
  ( MonadPlus m
  , MonadError String m
  ) => InvertibleGrammar m SexpGrammar where

  parseWithGrammar (GAtom g) (s :- t) =
    case s of
      Atom _ a -> parseWithGrammar g (a :- t)
      other    -> posError other "atom"
  parseWithGrammar (GList g) (s :- t) = do
    case s of
      List _ xs -> parseSeq xs g t
      other     -> posError other "list"
  parseWithGrammar (GVect g) (s :- t) = do
    case s of
      Vector _ xs -> parseSeq xs g t
      other       -> posError other "vector"

  genWithGrammar (GAtom g) t = do
    (a :- t') <- genWithGrammar g t
    return (Atom dummyPos a :- t')
  genWithGrammar (GList g) t = do
    (t', SeqCtx xs) <- runStateT (genWithGrammar g t) (SeqCtx [])
    return (List dummyPos xs :- t')
  genWithGrammar (GVect g) t = do
    (t', SeqCtx xs) <- runStateT (genWithGrammar g t) (SeqCtx [])
    return (Vector dummyPos xs :- t')

data AtomGrammar a b where
  GSym     :: Text -> AtomGrammar (Atom :- t) t
  GKw      :: Kw -> AtomGrammar (Atom :- t) t
  GBool    :: AtomGrammar (Atom :- t) (Bool :- t)
  GInt     :: AtomGrammar (Atom :- t) (Integer :- t)
  GReal    :: AtomGrammar (Atom :- t) (Scientific :- t)
  GString  :: AtomGrammar (Atom :- t) (Text :- t)
  GSymbol  :: AtomGrammar (Atom :- t) (Text :- t)
  GKeyword :: AtomGrammar (Atom :- t) (Kw :- t)

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

  genWithGrammar (GSym sym) t      = return (AtomSymbol sym :- t)
  genWithGrammar (GKw kw) t        = return (AtomKeyword kw :- t)
  genWithGrammar GBool (a :- t)    = return (AtomBool a :- t)
  genWithGrammar GInt (a :- t)     = return (AtomInt a :- t)
  genWithGrammar GReal (a :- t)    = return (AtomReal a :- t)
  genWithGrammar GString (a :- t)  = return (AtomString a :- t)
  genWithGrammar GSymbol (a :- t)  = return (AtomSymbol a :- t)
  genWithGrammar GKeyword (a :- t) = return (AtomKeyword a :- t)


data SeqGrammar a b where
  -- | Dispatch single list element with a grammar
  GElem :: Grammar SexpGrammar (Sexp :- t) t'
        -- ^ Grammar to parse list element at current position with
        -> SeqGrammar t t'

  GRest :: Grammar SexpGrammar (Sexp :- t) (a :- t)
        -> SeqGrammar t ([a] :- t)

  GProps :: Grammar PropGrammar t t'
         -> SeqGrammar t t'

newtype SeqCtx = SeqCtx { getItems :: [Sexp] }

instance
  ( MonadPlus m
  , MonadState SeqCtx m
  , MonadError String m
  ) => InvertibleGrammar m SeqGrammar where
  parseWithGrammar (GElem g) t = do
    xs <- gets getItems
    case xs of
      [] -> throwError $ "Unexpected end of sequence"
      x:xs' -> do
        modify $ \s -> s { getItems = xs' }
        parseWithGrammar g (x :- t)
  parseWithGrammar (GRest g) t = do
    xs <- gets getItems
    modify $ \s -> s { getItems = [] }
    go xs t
    where
      go []     t = return $ [] :- t
      go (x:xs) t = do
        y  :- t'  <- parseWithGrammar g (x :- t)
        ys :- t'' <- go xs t'
        return $ (y:ys) :- t''

  parseWithGrammar (GProps g) t = do
    xs <- gets getItems
    modify $ \s -> s { getItems = [] }
    props <- go xs M.empty
    evalStateT (parseWithGrammar g t) (PropCtx props)
    where
      go [] props = return props
      go (Atom _ (AtomKeyword kwd):x:xs) props = go xs (M.insert kwd x props)
      go other _ = throwError $ "Property-list is malformed: " ++ Lazy.unpack (printSexp (List dummyPos other))

  genWithGrammar (GElem g) t = do
    (x :- t') <- genWithGrammar g t
    modify $ \s -> s { getItems = x : getItems s }
    return t'
  genWithGrammar (GRest g) (ys :- t) = do
    xs :- t' <- go ys t
    put (SeqCtx xs)
    return t'
    where
      go []     t = return $ [] :- t
      go (y:ys) t = do
        x  :- t'  <- genWithGrammar g (y :- t)
        xs :- t'' <- go ys t'
        return $ (x : xs) :- t''
  genWithGrammar (GProps g) t = do
    (t', PropCtx props) <- runStateT (genWithGrammar g t) (PropCtx M.empty)
    let plist = foldr (\(name, sexp) acc -> Atom dummyPos (AtomKeyword name) : sexp : acc) [] (M.toList props)
    put $ SeqCtx plist
    return t'

newtype PropCtx = PropCtx { getProps :: Map Kw Sexp }

data PropGrammar a b where
  GProp :: Kw
        -> Grammar SexpGrammar (Sexp :- t) t'
        -> PropGrammar t t'

instance
  ( MonadPlus m
  , MonadState PropCtx m
  , MonadError String m
  ) => InvertibleGrammar m PropGrammar where
  parseWithGrammar (GProp kwd g) t = do
    ps <- gets getProps
    case M.lookup kwd ps of
      Nothing -> throwError $ "Keyword " ++ show kwd ++ " not found"
      Just x  -> parseWithGrammar g $ x :- t

  genWithGrammar (GProp kwd g) t = do
    x :- t' <- genWithGrammar g t
    modify $ \ps -> ps { getProps = M.insert kwd x (getProps ps) }
    return t'

parse
  :: (Functor m, MonadPlus m, MonadError String m, InvertibleGrammar m g)
  => Grammar g (Sexp :- ()) (a :- ())
  -> Sexp
  -> m a
parse gram input =
  (\(x :- _) -> x) <$> parseWithGrammar gram (input :- ())

gen
  :: (Functor m, MonadPlus m, MonadError String m, InvertibleGrammar m g)
  => Grammar g (Sexp :- ()) (a :- ())
  -> a
  -> m Sexp
gen gram input =
  (\(x :- _) -> x) <$> genWithGrammar gram (input :- ())
