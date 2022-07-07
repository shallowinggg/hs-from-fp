module Testing.WordNumber where

import Data.List (intersperse)

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
