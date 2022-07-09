module Applicative.ValidationTest where

import Applicative.Validation (Validation')
import Functor.Law (functorCompose, functorIdentity)
import Test.QuickCheck (Arbitrary, frequency, quickCheck)
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (applicative)

qcFunctorId = do
  quickCheck (functorIdentity :: Validation' [String] Int -> Bool)

qcFunctorCompose = do
  quickCheck (functorCompose (* 2) (+ 1) :: Validation' Int Int -> Bool)

x :: Validation' [String] (String, String, Int)
x = undefined

qcApplicative = do
  quickBatch $ applicative (undefined :: Validation' [String] (String, String, Int))
