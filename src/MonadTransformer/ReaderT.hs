{-# LANGUAGE InstanceSigs #-}

module MonadTransformer.ReaderT where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap :: Functor m => (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: Applicative m => a -> ReaderT r m a
  pure a = ReaderT (pure (pure a))

  (<*>) ::
    Applicative m =>
    ReaderT r m (a -> b) ->
    ReaderT r m a ->
    ReaderT r m b
  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return :: Monad m => a -> ReaderT r m a
  return = pure
  (>>=) :: Monad m => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO