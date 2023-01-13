module Reader.WarmUp where

import Data.Char (toUpper)

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  l <- cap
  r <- rev
  return (l, r)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' =
  cap
    >>= \l ->
      rev
        >>= \r -> return (l, r)
