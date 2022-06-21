module Lists.Cipher where

import Data.Char (chr, ord)

shift :: Char -> Int -> Char
shift c l
  | 'A' <= c && c <= 'Z' = go c l (ord 'A')
  | 'a' <= c && c <= 'z' = go c l (ord 'a')
  | otherwise = ' '
  where
    go c l start = chr (mod (ord c - start + l) 26 + start)

caesar :: Int -> [Char] -> [Char]
caesar _ "" = ""
caesar l (c : cs) = shift c l : caesar l cs

deCaesar :: Int -> [Char] -> [Char]
deCaesar _ "" = ""
deCaesar l (c : cs) = shift c (negate l) : deCaesar l cs
