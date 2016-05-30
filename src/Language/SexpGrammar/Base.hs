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
  , AtomGrammar (..)
  , SeqGrammar (..)
  , PropGrammar (..)
  , runParse
  , runGen
  , SexpG
  , SexpG_
  , module Data.InvertibleGrammar
  ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Reader
import Control.Monad.State

import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (singleton)
import Data.InvertibleGrammar.Monad

import Text.PrettyPrint.Leijen.Text (Pretty(..), hsep, displayT, renderPretty)

import Data.InvertibleGrammar
import Language.Sexp.Types
import Language.Sexp.Pretty ()

display :: (Pretty a) => a -> String
display = Lazy.unpack . displayT . renderPretty 0.5 75 . pretty

-- | Grammar which matches Sexp to a value of type a and vice versa.
type SexpG a = forall t. Grammar SexpGrammar (Sexp :- t) (a :- t)

-- | Grammar which pattern matches Sexp and produces nothing, or
-- consumes nothing but generates some Sexp.
type SexpG_ = forall t. Grammar SexpGrammar (Sexp :- t) t

throwErrorPos :: (MonadContextError Propagation GrammarError m, MonadReader Position m) => String -> m a
throwErrorPos msg = do
  Position line col <- ask
  grammarError $ Mismatch mempty (Just $ concat [show line, ":", show col, ": " ++ msg])

----------------------------------------------------------------------
-- Top-level grammar

data SexpGrammar a b where
  GAtom :: Grammar AtomGrammar (Atom :- t) t' -> SexpGrammar (Sexp :- t) t'
  GList :: Grammar SeqGrammar t            t' -> SexpGrammar (Sexp :- t) t'
  GVect :: Grammar SeqGrammar t            t' -> SexpGrammar (Sexp :- t) t'

unexpectedSexpError :: (MonadContextError Propagation GrammarError m) => String -> Sexp -> m a
unexpectedSexpError expected sexp =
  grammarError $ Mismatch (singleton expected) (Just $ display sexp)

instance
  ( MonadPlus m
  , MonadContextError Propagation GrammarError m
  ) => InvertibleGrammar m SexpGrammar where
  forward (GAtom g) (s :- t) =
    case s of
      Atom p a    -> runReaderT (dive $ forward g (a :- t)) p
      other       -> unexpectedSexpError "atom" other

  forward (GList g) (s :- t) = do
    case s of
      List p xs   -> runReaderT (dive $ parseSequence xs g t) p
      other       -> unexpectedSexpError "list" other

  forward (GVect g) (s :- t) = do
    case s of
      Vector p xs -> runReaderT (dive $ parseSequence xs g t) p
      other       -> unexpectedSexpError "vector" other


  backward (GAtom g) t = do
    (a :- t') <- runReaderT (dive $ backward g t) dummyPos
    return (Atom dummyPos a :- t')

  backward (GList g) t = do
    (t', SeqCtx xs) <- runStateT (runReaderT (dive $ backward g t) dummyPos) (SeqCtx [])
    return (List dummyPos xs :- t')

  backward (GVect g) t = do
    (t', SeqCtx xs) <- runStateT (runReaderT (dive $ backward g t) dummyPos) (SeqCtx [])
    return (Vector dummyPos xs :- t')

----------------------------------------------------------------------
-- Atom grammar

data AtomGrammar a b where
  GSym     :: Text -> AtomGrammar (Atom :- t) t
  GKw      :: Kw   -> AtomGrammar (Atom :- t) t
  GBool    :: AtomGrammar (Atom :- t) (Bool :- t)
  GInt     :: AtomGrammar (Atom :- t) (Integer :- t)
  GReal    :: AtomGrammar (Atom :- t) (Scientific :- t)
  GString  :: AtomGrammar (Atom :- t) (Text :- t)
  GSymbol  :: AtomGrammar (Atom :- t) (Text :- t)
  GKeyword :: AtomGrammar (Atom :- t) (Kw :- t)

unexpectedAtomError :: (MonadReader Position m, MonadContextError Propagation GrammarError m) => Atom -> Atom -> m a
unexpectedAtomError expected atom = do
  pos <- ask
  unexpectedSexpError (display $ Atom pos expected) (Atom pos atom)

unexpectedAtomTypeError :: (MonadReader Position m, MonadContextError Propagation GrammarError m) => String -> Atom -> m a
unexpectedAtomTypeError expected atom = do
  pos <- ask
  unexpectedSexpError ("atom of type " ++ expected) (Atom pos atom)

instance
  ( MonadPlus m
  , MonadContextError Propagation GrammarError m
  , MonadReader Position m
  ) => InvertibleGrammar m AtomGrammar where
  forward (GSym sym') (atom :- t) =
    case atom of
      AtomSymbol sym | sym' == sym -> return t
      _ -> unexpectedAtomError (AtomSymbol sym') atom

  forward (GKw kw') (atom :- t) =
    case atom of
      AtomKeyword kw | kw' == kw -> return t
      _ -> unexpectedAtomError (AtomKeyword kw') atom

  forward GBool (atom :- t) =
    case atom of
      AtomBool a -> return $ a :- t
      _          -> unexpectedAtomTypeError "bool" atom

  forward GInt (atom :- t) =
    case atom of
      AtomInt a -> return $ a :- t
      _         -> unexpectedAtomTypeError "int"  atom

  forward GReal (atom :- t) =
    case atom of
      AtomReal a -> return $ a :- t
      _          -> unexpectedAtomTypeError "real" atom

  forward GString (atom :- t) =
    case atom of
      AtomString a -> return $ a :- t
      _            -> unexpectedAtomTypeError "string" atom

  forward GSymbol (atom :- t) =
    case atom of
      AtomSymbol a -> return $ a :- t
      _            -> unexpectedAtomTypeError "symbol" atom

  forward GKeyword (atom :- t) =
    case atom of
      AtomKeyword a -> return $ a :- t
      _             -> unexpectedAtomTypeError "keyword" atom


  backward (GSym sym) t      = return (AtomSymbol sym :- t)
  backward (GKw kw) t        = return (AtomKeyword kw :- t)
  backward GBool (a :- t)    = return (AtomBool a :- t)
  backward GInt (a :- t)     = return (AtomInt a :- t)
  backward GReal (a :- t)    = return (AtomReal a :- t)
  backward GString (a :- t)  = return (AtomString a :- t)
  backward GSymbol (a :- t)  = return (AtomSymbol a :- t)
  backward GKeyword (a :- t) = return (AtomKeyword a :- t)


-----------------------------------------------------------------------
-- Sequence grammar

parseSequence :: (MonadContextError Propagation GrammarError m, MonadReader Position m, InvertibleGrammar (StateT SeqCtx m) g) => [Sexp] -> g a b -> a -> m b
parseSequence xs g t = do
  (a, SeqCtx rest) <- runStateT (forward g t) (SeqCtx xs)
  unless (null rest) $
    grammarError $ Mismatch mempty (Just $ "unexpected leftover elements: " ++ display (hsep (map pretty rest)))
  return a

data SeqGrammar a b where
  GElem :: Grammar SexpGrammar (Sexp :- t) t'
        -> SeqGrammar t t'

  GRest :: Grammar SexpGrammar (Sexp :- t) (a :- t)
        -> SeqGrammar t ([a] :- t)

  GProps :: Grammar PropGrammar t t'
         -> SeqGrammar t t'

newtype SeqCtx = SeqCtx { getItems :: [Sexp] }

instance
  ( MonadPlus m
  , MonadState SeqCtx m
  , MonadReader Position m
  , MonadContextError Propagation GrammarError m
  ) => InvertibleGrammar m SeqGrammar where
  forward (GElem g) t = do
    step
    xs <- gets getItems
    case xs of
      []    -> throwErrorPos "unexpected end of sequence"
      x:xs' -> do
        modify $ \s -> s { getItems = xs' }
        forward g (x :- t)

  forward (GRest g) t = do
    xs <- gets getItems
    modify $ \s -> s { getItems = [] }
    go xs t
    where
      go []     t = return $ [] :- t
      go (x:xs) t = do
        step
        y  :- t'  <- forward g (x :- t)
        ys :- t'' <- go xs t'
        return $ (y:ys) :- t''

  forward (GProps g) t = do
    xs <- gets getItems
    modify $ \s -> s { getItems = [] }
    props <- go xs M.empty
    (res, PropCtx ctx) <- runStateT (forward g t) (PropCtx props)
    when (not $ M.null ctx) $
      throwErrorPos $ "property-list contains unrecognized keys: " ++ unwords (map display (M.keys ctx))
    return res
    where
      go [] props = return props
      go (Atom _ (AtomKeyword kwd):x:xs) props = step >> go xs (M.insert kwd x props)
      go other _ = throwErrorPos $ "property-list is malformed: " ++ display (List dummyPos other)

  backward (GElem g) t = do
    step
    (x :- t') <- backward g t
    modify $ \s -> s { getItems = x : getItems s }
    return t'

  backward (GRest g) (ys :- t) = do
    xs :- t' <- go ys t
    put (SeqCtx xs)
    return t'
    where
      go []     t = return $ [] :- t
      go (y:ys) t = do
        step
        x  :- t'  <- backward g (y :- t)
        xs :- t'' <- go ys t'
        return $ (x : xs) :- t''

  backward (GProps g) t = do
    step
    (t', PropCtx props) <- runStateT (backward g t) (PropCtx M.empty)
    let plist = foldr (\(name, sexp) acc -> Atom dummyPos (AtomKeyword name) : sexp : acc) [] (M.toList props)
    put $ SeqCtx plist
    return t'

----------------------------------------------------------------------
-- Property list grammar

data PropGrammar a b where
  GProp    :: Kw
           -> Grammar SexpGrammar (Sexp :- t) (a :- t)
           -> PropGrammar t (a :- t)

  GOptProp :: Kw
           -> Grammar SexpGrammar (Sexp :- t) (a :- t)
           -> PropGrammar t (Maybe a :- t)

newtype PropCtx = PropCtx { getProps :: Map Kw Sexp }

instance
  ( MonadPlus m
  , MonadState PropCtx m
  , MonadReader Position m
  , MonadContextError Propagation GrammarError m
  ) => InvertibleGrammar m PropGrammar where
  forward (GProp kwd g) t = do
    ps <- gets getProps
    case M.lookup kwd ps of
      Nothing -> throwErrorPos $ "key " ++ display kwd ++ " not found"
      Just x  -> do
        put (PropCtx $ M.delete kwd ps)
        forward g $ x :- t

  forward (GOptProp kwd g) t = do
    ps <- gets getProps
    case M.lookup kwd ps of
      Nothing ->
        return (Nothing :- t)
      Just x  -> do
        put (PropCtx $ M.delete kwd ps)
        (a :- t') <- forward g (x :- t)
        return (Just a :- t')


  backward (GProp kwd g) t = do
    x :- t' <- backward g t
    modify $ \ps -> ps { getProps = M.insert kwd x (getProps ps) }
    return t'

  backward (GOptProp _ _) (Nothing :- t) = do
    return t

  backward (GOptProp kwd g) (Just x :- t) = do
    x' :- t' <- backward g (x :- t)
    modify $ \ps -> ps { getProps = M.insert kwd x' (getProps ps) }
    return t'

runParse
  :: (Functor m, MonadPlus m, MonadContextError Propagation GrammarError m, InvertibleGrammar m g)
  => Grammar g (Sexp :- ()) (a :- ())
  -> Sexp
  -> m a
runParse gram input =
  (\(x :- _) -> x) <$> forward gram (input :- ())

runGen
  :: (Functor m, MonadPlus m, MonadContextError Propagation GrammarError m, InvertibleGrammar m g)
  => Grammar g (Sexp :- ()) (a :- ())
  -> a
  -> m Sexp
runGen gram input =
  (\(x :- _) -> x) <$> backward gram (input :- ())
