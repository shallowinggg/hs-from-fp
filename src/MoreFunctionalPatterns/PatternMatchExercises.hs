module MoreFunctionalPatterns.PatternMatchExercises where

-- p1

k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k (4 - 1, 10)

k2 :: [Char]
k2 = k ("three", 1 + 2)

k3 :: Num a => a
k3 = k (3, True)

-- a) k :: (a, b) -> a
-- b) k2 :: [Char]
--    k3 :: Num a => a
--    k1 :: Num a => a
-- c) k1 and k3

-- p2

f ::
  (a, b, c) ->
  (d, e, f) ->
  ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
