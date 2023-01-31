{-# LANGUAGE InstanceSigs #-}

module MonadTransformer.EitherT where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Trans.Class (MonadTrans (lift))
import GHC.Base (liftM)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Functor m) => Functor (EitherT e m) where
  fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure :: Applicative m => a -> EitherT e m a
  pure a = EitherT $ pure (pure a)

  (<*>) ::
    Applicative m =>
    EitherT e m (a -> b) ->
    EitherT e m a ->
    EitherT e m b
  (EitherT fab) <*> (EitherT mea) = EitherT $ liftA2 (<*>) fab mea

instance Monad m => Monad (EitherT e m) where
  return :: Monad m => a -> EitherT e m a
  return = pure

  (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Right a -> runEitherT $ f a
      Left e -> return $ Left e

swapEither :: Either e a -> Either a e
swapEither a = case a of
  Left e -> Right e
  Right a -> Left a

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT ::
  Monad m =>
  (a -> m c) ->
  (b -> m c) ->
  EitherT a m b ->
  m c
eitherT f g (EitherT mab) = mab >>= either f g

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . liftM Right