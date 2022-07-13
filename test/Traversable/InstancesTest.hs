module Traversable.InstancesTest where

import GHC.Base (undefined)
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (functor, traversable)
import Traversable.Instances (Big, Bigger, Constant, Identity, List, Optional, Pair, S, Three)

type IIIS = (Maybe Int, Maybe Int, Maybe Int, String)

qcIdentity = do
  quickBatch $ functor (undefined :: Identity (String, String, Int))
  quickBatch $ traversable (undefined :: Identity ([Int], [Int], [Int], String))

qcInstances = do
  quickBatch $ traversable (undefined :: Identity IIIS)
  quickBatch $ traversable (undefined :: Constant Int IIIS)
  quickBatch $ traversable (undefined :: Optional IIIS)
  quickBatch $ traversable (undefined :: List IIIS)
  quickBatch $ traversable (undefined :: Three Int Int IIIS)
  quickBatch $ traversable (undefined :: Pair Int IIIS)
  quickBatch $ traversable (undefined :: Big Int IIIS)
  quickBatch $ traversable (undefined :: Bigger Int IIIS)
  quickBatch $ traversable (undefined :: S Maybe IIIS)
