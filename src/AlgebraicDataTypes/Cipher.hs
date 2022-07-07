module AlgebraicDataTypes.Cipher where

import Data.Char (chr, ord)

shift :: Char -> Int -> Char
shift c l
  | 'A' <= c && c <= 'Z' = go c l (ord 'A')
  | 'a' <= c && c <= 'z' = go c l (ord 'a')
  | otherwise = ' '
  where
    go c l start = chr (mod (ord c - start + l) 26 + start)

complete xs l =
  if len < l
    then xs ++ complete xs (l - len)
    else xs
  where
    len = length xs

go _ _ "" = ""
go _ _ " " = " "
go _ "" text = text
go f ks (' ' : cs) = ' ' : go f ks cs
go f (k : ks) (c : cs) = f c k : go f ks cs

vigenere :: [Char] -> [Char] -> [Char]
vigenere key text = go (\c k -> shift c (ord k - ord 'A')) newKey text
  where
    newKey = complete key (length text)

deVigenere :: [Char] -> [Char] -> [Char]
deVigenere key text = go (\c k -> shift c (ord 'A' - ord c)) newKey text
  where
    newKey = complete key (length text)
