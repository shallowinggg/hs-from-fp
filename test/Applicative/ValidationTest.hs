module Applicative.ValidationTest where

import Applicative.Validation (Validation')
import Test.QuickCheck (Arbitrary, frequency, quickCheck)
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (applicative)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

qcFunctorId = do
  quickCheck (functorIdentity :: Validation' [String] Int -> Bool)

qcFunctorCompose = do
  quickCheck (functorCompose (* 2) (+ 1) :: Validation' Int Int -> Bool)

x :: Validation' [String] (String, String, Int)
x = undefined

qcApplicative = do
  quickBatch $ applicative x
