module Lists.Exercises where

-- Exercise: EnumFromTo

myFromThenTo :: (Enum a) => a -> a -> [a]
myFromThenTo a b
  | aIdx > bIdx = []
  | aIdx == bIdx = [a]
  | otherwise = a : myFromThenTo (succ a) b
  where
    aIdx = fromEnum a
    bIdx = fromEnum b

eftBool :: Bool -> Bool -> [Bool]
eftBool = myFromThenTo

eftOrd ::
  Ordering ->
  Ordering ->
  [Ordering]
eftOrd = myFromThenTo

eftInt :: Int -> Int -> [Int]
eftInt = myFromThenTo

eftChar :: Char -> Char -> [Char]
eftChar = myFromThenTo

-- Exercises: Thy Fearful Symmetry

myWords :: String -> [String]
myWords "" = []
myWords x = takeWhile (/= ' ') y : myWords (dropWhile (/= ' ') y)
  where
    y = dropWhile (== ' ') x

mySplit :: (Char -> Bool) -> String -> [String]
mySplit _ "" = []
mySplit f x = takeWhile f y : myWords (dropWhile f y)
  where
    y = dropWhile (not . f) x

myWords' = mySplit (/= ' ')

-- Exercises: Comprehend Thy Lists

-- 1. [4, 16, 36, 64, 100]
-- 2. [(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]
-- 3. [(1,64),(1,81),(1,100),(4,64),(4,81)]

-- Exercises: Square Cube

mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [y ^ 3 | y <- [1 .. 5]]

-- 1.
p1 = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
p2 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

-- 3.
p3 = length p2

-- Exercises: Bottom Madness

-- Will it blow up?
-- 1. Y
-- 2. N
-- 3. Y
-- 4. N
-- 5. Y
-- 6. N
-- 7. Y
-- 8. N
-- 9. N
-- 10. Y

-- Intermission: Is it in normal form?
-- 1. 1
-- 2. 2
-- 3. 3
-- 4. 3
-- 5. 3
-- 6. 3
-- 7. 2

-- Exercises: More Bottoms

-- 1. bottom
-- 2. Y
-- 3. N
-- 4. itIsMystery :: [Char] -> [Bool]
-- 5. a) [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
--    b) [1, 10, 20]
--    c) [15, 15, 15]
-- 6.