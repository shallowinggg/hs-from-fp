module Testing.ArithTest where

import Data.List (sort)
import Foreign (toBool)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, forAll, quickCheck)
import Testing.Arith (half, listOrdered)
import Text.Read (Lexeme (String))

-- 1.

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_half :: (Eq a, Fractional a) => a -> Bool
prop_half x = x == halfIdentity x

qcHalf :: IO ()
qcHalf = do
  quickCheck (prop_half :: Float -> Bool)
  quickCheck (prop_half :: Double -> Bool)

-- 2.

prop_sortedListOrdered :: (Ord a) => [a] -> Bool
prop_sortedListOrdered = listOrdered . sort

qcSortedListOrdered :: IO ()
qcSortedListOrdered = do
  quickCheck (prop_sortedListOrdered :: [Int] -> Bool)
  quickCheck (prop_sortedListOrdered :: String -> Bool)

-- 3.

prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

qcPlus :: IO ()
qcPlus = do
  quickCheck (prop_plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_plusCommutative :: Int -> Int -> Bool)

-- 4.

prop_mulAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_mulAssociative x y z = x * (y * z) == (x * y) * z

prop_mulCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_mulCommutative x y = (x * y) == (y * x)

qcMul :: IO ()
qcMul = do
  quickCheck (prop_mulAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_mulCommutative :: Int -> Int -> Bool)

-- 5.

prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem x y = y == 0 || quot x y * y + rem x y == x

prop_divMod :: Integral a => a -> a -> Bool
prop_divMod x y = y == 0 || div x y * y + mod x y == x

qcQuotRemAndDivMod :: IO ()
qcQuotRemAndDivMod = do
  quickCheck (prop_quotRem :: Int -> Int -> Bool)
  quickCheck (prop_divMod :: Int -> Int -> Bool)

-- 6

-- 0 0 0 will fail
prop_exponentAssociative :: (Integral b1, Integral b2, Num a, Eq a) => a -> b1 -> b2 -> Bool
prop_exponentAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_exponentCommutative :: Integral b => b -> b -> Bool
prop_exponentCommutative x y = x ^ y == y ^ x

qcExponent :: IO ()
qcExponent = do
  -- quickCheck (prop_exponentCommutative :: Int -> Int -> Bool)
  quickCheck (prop_exponentAssociative :: Int -> Int -> Int -> Bool)

-- 7

prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = (reverse $ reverse xs) == xs

qcReverse :: IO ()
qcReverse = do
  quickCheck (prop_reverse :: [Int] -> Bool)
  quickCheck (prop_reverse :: String -> Bool)

-- 8

prop_dollar :: Eq a => (t -> a) -> t -> a -> Bool
prop_dollar f a b = f a == b

-- 9.

prop_foldrPP :: Eq a => [a] -> [a] -> Bool
prop_foldrPP xs ys = foldr (:) xs ys == ys ++ xs

prop_foldrConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

qcFoldr :: IO ()
qcFoldr = do
  quickCheck (prop_foldrPP :: [Int] -> [Int] -> Bool)
  quickCheck (prop_foldrConcat :: [[Int]] -> Bool)

-- 10

f :: Int -> [a] -> Int
f n xs = length (take n xs)

prop_f :: Int -> [a] -> Bool
prop_f n xs = f n xs == n

qcF :: IO ()
qcF = do
  -- when n > length xs, this will fail
  quickCheck (prop_f :: Int -> [Int] -> Bool)

-- 11

g :: (Read a1, Show a2) => a2 -> a1
g x = read (show x)

prop_g :: (Eq a, Read a, Show a) => a -> Bool
prop_g x = g x == x

qcG :: IO ()
qcG = do
  quickCheck (prop_g :: Int -> Bool)

-- Failure

square :: Num a => a -> a
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x = squareIdentity x == x

qcSquareIdentity = do
  quickCheck (prop_squareIdentity :: Double -> Bool)
