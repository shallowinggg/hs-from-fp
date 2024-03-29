{-# LANGUAGE InstanceSigs #-}

module ComposingTypes.Twinplicative where

import Data.Functor.Identity (Identity (Identity))
import GHC.Base (Applicative (liftA2))

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (Compose f) <*> (Compose fga) = Compose $ liftA2 (<*>) f fga
  (Compose f) <*> (Compose fga) =
    Compose $
      let a = (<*>) <$> f
       in a <*> fga
