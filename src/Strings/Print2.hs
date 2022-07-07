module Strings.Print2 where

print2 :: IO ()
print2 = do
  putStrLn "Count to four:"
  putStr "one, two"
  putStr ", three, and"
  putStrLn " four!"