{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Transformers where

import           Prelude (Either (..), Maybe (..), Monoid (..), const, ($), (.))

class MonadTrans t where
  lift :: Monad m => m a -> t m a

class Monad m where
    (>>=)       :: m a -> (a -> m b) -> m b
    return      :: a -> m a

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= f  = StateT $ \s -> runStateT m s >>= (\(val, newS) -> runStateT (f val) newS)

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> m >>= (\v -> return (v, s))

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= f  = WriterT $ runWriterT m >>= (\(v, w) -> runWriterT (f v) >>= (\(v2, w2) -> return (v2, w `mappend` w2)))

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ m >>= (\a -> return (a, mempty))

instance Monad m => Monad (EitherT l m) where
    return x = EitherT $ return $ Right x
    m >>= f = EitherT $ runEitherT m >>= g
              where g (Left l)  = return $ Left l
                    g (Right r) = runEitherT $ f r

instance MonadTrans (EitherT l) where
    lift m = EitherT $ m >>= (return . Right)

instance MonadTrans (ReaderT r)  where
   lift m = ReaderT (const m)

instance (Monad m) => Monad (ReaderT r m) where
    return  = lift . return
    m >>= f = ReaderT $ \r ->
        runReaderT m r >>= (\a ->
        runReaderT (f a) r)

instance (Monad m) => Monad (MaybeT m) where
    return a = MaybeT $ return $ Just a
    x >>= f = MaybeT $
        runMaybeT x >>= (\v ->
                           case v of
                             Nothing -> return Nothing
                             Just y  -> runMaybeT (f y))

instance MonadTrans MaybeT where
    lift m = MaybeT $ m >>= (return . Just)

class (Monad m) => (MonadState s m) where
    get :: m s
    put :: s -> m ()

instance Monad m => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance MonadState s m => MonadState s (ReaderT r m) where
    get   = lift get
    put s = lift $ put s

instance MonadState s m => MonadState s (MaybeT m) where
    get   = lift get
    put s = lift $ put s
