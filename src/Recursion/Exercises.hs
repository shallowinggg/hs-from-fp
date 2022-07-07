module Recursion.Exercises where

import Data.List (intersperse)

-- Review of types
-- 1. d
-- 2. b
-- 3. d
-- 4. b

-- Reviewing currying
-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"

-- Recursion
-- 1.
-- 2.
f :: (Eq a, Num a) => a -> a
f 1 = 1
f x = x + f (x - 1)

-- 3.
g :: (Integral a) => a -> a -> a
g x 1 = x
g x y = x + g x (y - 1)

-- Fixing dividedBy
data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | signum num == signum denom = Result (go num denom 0)
  | otherwise = Result (- (go (abs num) (abs denom) 0 + 1))
  where
    go n d count
      | n < d = count
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ (x + 11)

-- Numbers into words
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits x = go x []
  where
    go x xs
      | x < 10 = x : xs
      | otherwise = go (x `div` 10) ((x `mod` 10) : xs)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
