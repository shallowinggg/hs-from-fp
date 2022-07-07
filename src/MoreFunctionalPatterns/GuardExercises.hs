module MoreFunctionalPatterns.GuardExercises where

-- p1
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where
    y = x / 100

-- p3
-- b)
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- p4
-- Eq a => [a]

-- p5
-- Eq a => [a] -> Bool

-- p6
-- c)
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- p7
-- (Ord a, Num a) => a

-- p8
-- (Ord a, Num a, Num p) => a -> p
