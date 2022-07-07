module FoldingLists.Scans where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- 1.
f20 = take 20 fibs

-- 2.
-- filter without take will still get an infinite list
f100 = takeWhile (< 100) fibs

-- 3.
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' n = scanl (*) 1 [2 .. n]
