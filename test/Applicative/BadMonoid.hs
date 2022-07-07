module Applicative.BadMonoid where

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import Test.QuickCheck.Classes (monoid)

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency
      [ (1, return Fools),
        (2, return Twoo)
      ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

qcMonoid :: IO ()
qcMonoid = quickBatch (monoid Twoo)
