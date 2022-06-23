module FoldingList.Exercises where

-- Exercises: Understanding Folds

-- 1. b) c)

-- 2.
-- f = flip (*)
-- (((1 `f` 1) `f` 2) `f` 3)
-- (1 `f` 2) `f` 3
-- 2 `f` 3
-- 6

-- 3. c)

-- 4. a)

-- 5.
-- a) foldr (++) "" ["woot", "WOOT", "woot"]
-- b) foldr max ' ' "fear is the little death"
-- c) foldr (&&) True [False, True]
-- d) Yes
-- e) foldl (flip ((++) . show)) "" [1..5]
-- f) foldr const 0 [1..5]
-- g) foldr const 'a' "tacos"
-- h) foldl (flip const) 'a' "burritos"
-- i) foldl (flip const) 0 [1..5]

---
--- Rewriting functions using folds

-- 1.

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (== e)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldr (flip (++) . (: [])) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> ([x | f x]) ++ y) []

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (last xs) xs

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT then x else y) (last xs) xs
