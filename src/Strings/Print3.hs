module Strings.Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world"

print3 :: IO ()
print3 = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting =
      concat [hello, " ", world]
