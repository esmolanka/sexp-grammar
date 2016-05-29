{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.InvertibleGrammar.Monad
  ( ContextErrorT
  , runContextErrorT
  , ContextError
  , runContextError
  , MonadContextError (..)
  ) where

import Control.Applicative

import Control.Monad

import Control.Monad.Trans
import Control.Monad.Trans.Cont
#if MIN_VERSION_transformers(3, 0, 0)
import Control.Monad.Trans.Error
#else
import Control.Monad.Trans.Except
#endif
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)

import Control.Monad.State  (MonadState (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer (MonadWriter (..))

import Data.Functor.Identity
import Data.Semigroup

----------------------------------------------------------------------
-- Monad

newtype ContextErrorT c e m a =
  ContextErrorT { unContextErrorT :: forall b. c -> (e -> m b) -> (a -> m b) -> m b }

runContextErrorT :: (Monad m) => ContextErrorT c e m a -> c -> m (Either e a)
runContextErrorT k c = unContextErrorT k c (return . Left) (return . Right)

type ContextError c e a = ContextErrorT c e Identity a

runContextError :: ContextError c e a -> c -> Either e a
runContextError k c = runIdentity $ unContextErrorT k c (return . Left) (return . Right)

instance Functor (ContextErrorT c e m) where
  fmap f e = ContextErrorT $ \c err ret -> unContextErrorT e c err (ret . f)

instance Applicative (ContextErrorT c e m) where
  pure a = ContextErrorT $ \_ _ ret -> ret a
  {-# INLINE pure #-}

  fe <*> ae = ContextErrorT $ \c err ret ->
    unContextErrorT fe c err (\f -> unContextErrorT ae c err (ret . f))
  {-# INLINE (<*>) #-}

instance (Semigroup e) => Alternative (ContextErrorT c e m) where
  -- FIXME: sane 'empty' needed!
  empty = ContextErrorT $ \_ err _ -> err (error "empty ContextErrorT")
  {-# INLINE empty #-}

  ae <|> be = ContextErrorT $ \c err ret ->
    unContextErrorT ae c (\e -> unContextErrorT be c (\e' -> err (e <> e')) ret) ret
  {-# INLINE (<|>) #-}

instance Monad (ContextErrorT c e m) where
  return a = ContextErrorT $ \_ _ ret -> ret a
  {-# INLINE return #-}

  ma >>= fb =
    ContextErrorT $ \c err ret -> unContextErrorT ma c err $ \a ->
      unContextErrorT (fb a) c err ret
  {-# INLINE (>>=) #-}

instance (Semigroup e) => MonadPlus (ContextErrorT c e m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (ContextErrorT c e) where
  lift act = ContextErrorT $ \_ _ ret -> act >>= ret
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (ContextErrorT c e m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter w m => MonadWriter w (ContextErrorT c e m) where
  writer = lift . writer
  tell = lift . tell
  listen m = ContextErrorT $ \c err ret -> do
    res <- listen (runContextErrorT m c)
    case res of
      (Right a, w) -> ret (a, w)
      (Left e, _) -> err e
  pass m = ContextErrorT $ \c err ret -> pass $ do
    res <- runContextErrorT m c
    case res of
      Right (a, f) -> fmap (\b -> (b, f)) $ ret a
      Left e -> fmap (\b -> (b, id)) $ err e

instance MonadReader r m => MonadReader r (ContextErrorT c e m) where
  ask = lift ask
  local f m = ContextErrorT $ \c err ret ->
    local f (unContextErrorT m c err ret)
  reader = lift . reader

----------------------------------------------------------------------
-- Monad class stuff

class (Monad m) => MonadContextError c e m | m -> c e
  where
    throwInContext :: (c -> e) -> m a

instance Monad m =>
         MonadContextError c e (ContextErrorT c e m) where
  throwInContext f = ContextErrorT $ \c err _ -> err (f c)

instance MonadContextError c e m =>
         MonadContextError c e (ContT r m) where
    throwInContext = lift . throwInContext

#if MIN_VERSION_transformers(3, 0, 0)
instance (Error e', MonadContextError c e m) =>
         MonadContextError c e (ErrorT e' m) where
    throwInContext = lift . throwInContext
#else
instance MonadContextError c e m =>
         MonadContextError c e (ExceptT e m) where
    throwInContext = lift . throwInContext
#endif

instance MonadContextError c e m =>
         MonadContextError c e (IdentityT m) where
    throwInContext = lift . throwInContext

instance MonadContextError c e m =>
         MonadContextError c e (ListT m) where
    throwInContext = lift . throwInContext

instance MonadContextError c e m =>
         MonadContextError c e (MaybeT m) where
    throwInContext = lift . throwInContext

instance MonadContextError c e m =>
         MonadContextError c e (ReaderT r m) where
    throwInContext = lift . throwInContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Lazy.WriterT w m) where
    throwInContext = lift . throwInContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Strict.WriterT w m) where
    throwInContext = lift . throwInContext

instance MonadContextError c e m =>
         MonadContextError c e (Lazy.StateT s m) where
    throwInContext = lift . throwInContext

instance MonadContextError c e m =>
         MonadContextError c e (Strict.StateT s m) where
    throwInContext = lift . throwInContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Lazy.RWST r w s m) where
    throwInContext = lift . throwInContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Strict.RWST r w s m) where
    throwInContext = lift . throwInContext
