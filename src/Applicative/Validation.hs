module Applicative.Validation (Validation') where

import Test.QuickCheck (Arbitrary, frequency)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

data Validation' e a
  = Failure' e
  | Success' a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

-- This is different
instance Monoid e => Applicative (Validation' e) where
  pure = Success'
  (<*>) (Failure' e1) (Failure' e2) = Failure' $ e1 <> e2
  (<*>) (Failure' e) _ = Failure' e
  (<*>) _ (Failure' e) = Failure' e
  (<*>) (Success' f) (Success' a) = Success' $ f a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency
      [ (1, return (Failure' e)),
        (1, return (Success' a))
      ]

instance (Eq e, Eq a) => EqProp (Validation' e a) where
  (=-=) = eq
