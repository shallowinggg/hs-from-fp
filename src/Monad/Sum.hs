module Monad.Sum where

import Test.QuickCheck (Arbitrary (arbitrary), frequency)
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  (Second f) <*> (First a) = First a
  (Second f) <*> (Second b) = Second $ f b

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency
      [ (1, return $ First a),
        (2, return $ Second b)
      ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq
