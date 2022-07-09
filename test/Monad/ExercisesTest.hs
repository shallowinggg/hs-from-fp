module Monad.ExercisesTest where

import Monad.Exercises (Identity (Identity), List, Nope, PhhhbbtttEither, j)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad)

type SSI = (String, String, Int)

qcNope = do
  quickBatch $ functor (undefined :: Nope SSI)
  quickBatch $ applicative (undefined :: Nope SSI)
  quickBatch $ monad (undefined :: Nope SSI)

qcPhhhhbbtttEither = do
  quickBatch $ functor (undefined :: PhhhbbtttEither String SSI)
  quickBatch $ applicative (undefined :: PhhhbbtttEither String SSI)
  quickBatch $ monad (undefined :: PhhhbbtttEither String SSI)

qcIdentity = do
  quickBatch $ functor (undefined :: Identity SSI)
  quickBatch $ applicative (undefined :: Identity SSI)
  quickBatch $ monad (undefined :: Identity SSI)

qcList = do
  quickBatch $ functor (undefined :: List SSI)
  quickBatch $ applicative (undefined :: List SSI)
  quickBatch $ monad (undefined :: List SSI)

jSpec = hspec $ do
  describe "j" $ do
    it "j [[1, 2], [], [3]] = [1,2,3]" $ do
      j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
    it "j (Just (Just 1)) = Just 1" $ do
      j (Just (Just 1)) `shouldBe` Just 1
    it "j (Just Nothing) = Nothing" $ do
      j (Just Nothing) `shouldBe` (Nothing :: Maybe Integer)
    it "j Nothing = Nothing" $ do
      j Nothing `shouldBe` (Nothing :: Maybe Integer)
