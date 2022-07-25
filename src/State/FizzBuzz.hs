module State.FizzBuzz where

import Control.Monad.Trans.State (State, execState, get, put)

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

main1 :: IO ()
main1 =
  mapM_ putStrLn $
    reverse $ fizzbuzzList [1 .. 100]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo a b = fizzbuzzList [b, b - 1 .. a]
