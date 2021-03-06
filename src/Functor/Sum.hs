module Functor.Sum where

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
applyIfSecond = fmap

-- 2.
-- a is part of Functor
