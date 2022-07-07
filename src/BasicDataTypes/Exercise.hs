module BasicDataTypes.Exercise where

--p1
length' :: [a] -> Int
length' = undefined

--p4
x1 = div 6 (length [1, 2, 3])

-- p8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- p9
myAbs :: Integer -> Integer
myAbs x =
  if x > 0
    then x
    else negate x

-- p10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((snd (a, b), snd (c, d)), (fst (a, b), fst (c, d)))

-- or f (a, b) (c, d) = ((b, d), (a, c))

-- Correcting syntax

-- p1
x = (+)

p1f xs = w `x` 1
  where
    w = length xs

-- p2

-- p3
p3f (a, b) = a