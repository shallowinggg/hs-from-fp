{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module State.Moi where

import Data.Bifunctor (first)

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x -> let (a, s) = g x in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a,)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \x ->
    let (a, s) = g x
        (f2, s') = f x
     in (f2 a, s)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \x ->
    let (a, s) = f x
        Moi h = g a
     in h x
