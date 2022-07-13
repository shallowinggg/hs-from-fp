module Traversable.TreeTest where

import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (traversable)
import Traversable.Tree (Tree)

qcTree = do
  quickBatch $ traversable (undefined :: Tree (Maybe Int, Maybe Int, Maybe Int, String))