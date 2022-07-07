module Functor.HeavyLifting where

-- 1.
a :: [Int]
a = (+ 1) <$> read "[1]" :: [Int]

-- 2.
b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c :: Integer -> Integer
c = (* 2) . (\x -> x - 2)

-- 4.
d :: Integer -> [Char]
d = ((return '1' ++) . show) . (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
   in fmap (* 3) changed
