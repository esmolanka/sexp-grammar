{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.ContextError
  ( ContextErrorT
  , runContextErrorT
  , ContextError
  , runContextError
  , MonadContextError (..)
  ) where


#if MIN_VERSION_mtl(2, 2, 0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

import Control.Applicative
import Control.Monad.Trans.Cont as Cont (ContT, liftLocal)
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.List (ListT, mapListT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST, mapRWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST, mapRWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, mapStateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, mapStateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT, mapWriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT, mapWriterT)

import Control.Monad.State  (MonadState (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer (MonadWriter (..))

import Data.Functor.Identity
import Data.Semigroup

----------------------------------------------------------------------
-- Monad

newtype ContextErrorT c e m a =
  ContextErrorT { unContextErrorT :: forall b. c -> (e -> m b) -> (c -> a -> m b) -> m b }

runContextErrorT :: (Monad m) => ContextErrorT c e m a -> c -> m (Either e a)
runContextErrorT k c = unContextErrorT k c (return . Left) (const $ return . Right)

type ContextError c e a = ContextErrorT c e Identity a

runContextError :: ContextError c e a -> c -> Either e a
runContextError k c = runIdentity $ unContextErrorT k c (return . Left) (const $ return . Right)

instance Functor (ContextErrorT c e m) where
  fmap f e = ContextErrorT $ \c err ret -> unContextErrorT e c err (\c' -> ret c' . f)

instance Applicative (ContextErrorT c e m) where
  pure a = ContextErrorT $ \c _ ret -> ret c a
  {-# INLINE pure #-}

  fe <*> ae = ContextErrorT $ \c err ret ->
    unContextErrorT fe c err (\c' f -> unContextErrorT ae c' err (\c'' -> ret c'' . f))
  {-# INLINE (<*>) #-}

instance (Semigroup e) => Alternative (ContextErrorT c e m) where
  -- FIXME: sane 'empty' needed!
  empty = ContextErrorT $ \_ err _ -> err (error "empty ContextErrorT")
  {-# INLINE empty #-}

  ae <|> be = ContextErrorT $ \c err ret ->
    unContextErrorT ae c (\e -> unContextErrorT be c (\e' -> err (e <> e')) ret) ret
  {-# INLINE (<|>) #-}

instance Monad (ContextErrorT c e m) where
  return a = ContextErrorT $ \c _ ret -> ret c a
  {-# INLINE return #-}

  ma >>= fb =
    ContextErrorT $ \c err ret -> unContextErrorT ma c err $ \c' a ->
      unContextErrorT (fb a) c' err ret
  {-# INLINE (>>=) #-}

instance (Semigroup e) => MonadPlus (ContextErrorT c e m) where
  mzero = empty
  {-# INLINE mzero #-}

  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadTrans (ContextErrorT c e) where
  lift act = ContextErrorT $ \c _ ret -> act >>= ret c
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (ContextErrorT c e m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter w m => MonadWriter w (ContextErrorT c e m) where
  writer = lift . writer
  tell = lift . tell
  listen m = ContextErrorT $ \c err ret -> do
    (res, w) <- listen (unContextErrorT m c (return . Left) (curry (return . Right)))
    case res of
      Left e -> err e
      Right (c', a) -> ret c' (a, w)
  pass m = ContextErrorT $ \c err ret -> pass $ do
    res <- unContextErrorT m c (return . Left) (curry (return . Right))
    case res of
      Right (c', (a, f)) -> liftM (\b -> (b, f)) $ ret c' a
      Left e -> liftM (\b -> (b, id)) $ err e

instance MonadReader r m => MonadReader r (ContextErrorT c e m) where
  ask = lift ask
  local f m = ContextErrorT $ \c err ret ->
    local f (unContextErrorT m c err ret)
  reader = lift . reader

----------------------------------------------------------------------
-- Monad class stuff

class (Monad m) => MonadContextError c e m | m -> c e where
  throwInContext :: (c -> e) -> m a
  askContext     :: m c
  localContext  :: (c -> c) -> m a -> m a
  modifyContext   :: (c -> c) -> m ()

instance Monad m =>
         MonadContextError c e (ContextErrorT c e m) where
  throwInContext f = ContextErrorT $ \c err _ -> err (f c)
  askContext = ContextErrorT $ \c _ ret -> ret c c
  localContext f m = ContextErrorT $ \c err ret ->
    unContextErrorT m (f c) err (\_ -> ret c)
  modifyContext f = ContextErrorT $ \c _ ret -> ret (f c) ()

instance MonadContextError c e m =>
         MonadContextError c e (ContT r m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Cont.liftLocal askContext localContext
    modifyContext = lift . modifyContext

#if MIN_VERSION_mtl(2, 2, 0)

instance MonadContextError c e m =>
         MonadContextError c e (ExceptT e m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = mapExceptT . localContext
    modifyContext = lift . modifyContext

#else

instance (Error e', MonadContextError c e m) =>
         MonadContextError c e (ErrorT e' m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = mapErrorT . localContext
    modifyContext = lift . modifyContext

#endif

instance MonadContextError c e m =>
         MonadContextError c e (IdentityT m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = mapIdentityT . localContext
    modifyContext = lift . modifyContext

instance MonadContextError c e m =>
         MonadContextError c e (ListT m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = mapListT . localContext
    modifyContext = lift . modifyContext

instance MonadContextError c e m =>
         MonadContextError c e (MaybeT m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = mapMaybeT . localContext
    modifyContext = lift . modifyContext

instance MonadContextError c e m =>
         MonadContextError c e (ReaderT r m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = mapReaderT . localContext
    modifyContext = lift . modifyContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Lazy.WriterT w m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Lazy.mapWriterT . localContext
    modifyContext = lift . modifyContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Strict.WriterT w m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Strict.mapWriterT . localContext
    modifyContext = lift . modifyContext

instance MonadContextError c e m =>
         MonadContextError c e (Lazy.StateT s m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Lazy.mapStateT . localContext
    modifyContext = lift . modifyContext

instance MonadContextError c e m =>
         MonadContextError c e (Strict.StateT s m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Strict.mapStateT . localContext
    modifyContext = lift . modifyContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Lazy.RWST r w s m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Lazy.mapRWST . localContext
    modifyContext = lift . modifyContext

instance (Monoid w, MonadContextError c e m) =>
         MonadContextError c e (Strict.RWST r w s m) where
    throwInContext = lift . throwInContext
    askContext = lift askContext
    localContext = Strict.mapRWST . localContext
    modifyContext = lift . modifyContext
