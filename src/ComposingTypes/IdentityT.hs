{-# LANGUAGE InstanceSigs #-}

module ComposingTypes.IdentityT where

newtype IdentityT m a = IdentityT {runIdentityT :: m a}
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure :: Applicative m => a -> IdentityT m a
  pure x = IdentityT (pure x)
  (<*>) :: Applicative m => IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return :: Monad m => a -> IdentityT m a
  return = pure
  (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
