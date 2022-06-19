module MoreFunctionalPatterns.Exercises where

-- p1
-- 1. d
-- 2. b
-- 3. d
-- 4. b
-- 4. a

-- Let’s write code
-- p1
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit' :: Integral b => b -> b
tensDigit' x = d
  where
    (tens, _) = divMod x 10
    (_, d) = divMod tens 10

hunsD = tensDigit . (`div` 10)

xDigit x digit
  | x > 0 = xDigit (x - 1) (digit `div` 10)
  | x == 0 = digit `mod` 10
  | otherwise = digit

-- p2
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
  True -> x
  False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

-- p3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- p4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- roundTrip a = read (a -> String) a
--             = read String
--             = (String -> a) String
--             = a

main = do
  print (roundTrip 4)
  print (id 4)

-- p5
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- p6
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

-- print (roundTrip2 4::Int)
