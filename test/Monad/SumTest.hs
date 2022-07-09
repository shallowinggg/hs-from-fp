module Monad.SumTest where

import Monad.Sum (Sum)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad)

qcSum = do
  let trigger :: Sum String (String, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
