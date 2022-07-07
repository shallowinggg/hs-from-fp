module MoreFunctionalPatterns.CaseExercises where

-- p1
functionC :: Ord p => p -> p -> p
functionC x y =
  case x > y of
    True -> x
    False -> y

-- p2
ifEvenAdd2 :: Num p => p -> p
ifEvenAdd2 n =
  case even 2 of
    True -> n + 2
    False -> n

-- p3
nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1
