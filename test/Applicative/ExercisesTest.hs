module Applicative.ExercisesTest where

import Applicative.Exercises (Four (Four), Four' (Four'), Pair, Three, Three' (Three'), Two)
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (applicative)

type SSI = (String, String, Int)

qcPairApplicative = do
  quickBatch $ applicative (undefined :: Pair SSI)

qcTwoApplicative = do
  quickBatch $ applicative (undefined :: Two String SSI)

qcThreeApplicative = do
  quickBatch $ applicative (undefined :: Three String String SSI)

qcThree'Applicative = do
  quickBatch $ applicative (undefined :: Three' String SSI)

qcFourApplicative = do
  quickBatch $ applicative (undefined :: Four String String String SSI)

qcFour'Applicative = do
  quickBatch $ applicative (undefined :: Four' String SSI)
