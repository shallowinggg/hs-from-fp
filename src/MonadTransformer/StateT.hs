module MonadTransformer.StateT where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.Bifunctor

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ (fmap . fmap) (Data.Bifunctor.first f) g

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT f) <*> (StateT g) =
    StateT $ \s -> do
      v <- f s
      v' <- g (snd v)
      return (Data.Bifunctor.first (fst v) v')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO ia = StateT $ \s -> do
    a <- liftIO ia
    return (a, s)

f xs n
  | n < 0 = error "negIndex"
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> x
            _ -> r (k - 1)
        )
        (error "")
        xs
        n