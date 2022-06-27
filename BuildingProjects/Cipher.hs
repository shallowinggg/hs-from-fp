module BuildingProjects.Cipher where

import Data.Char (chr, digitToInt, ord)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

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

caesar :: Int -> [Char] -> [Char]
caesar _ "" = ""
caesar l (c : cs) = shift c l : caesar l cs

deCaesar :: Int -> [Char] -> [Char]
deCaesar _ "" = ""
deCaesar l (c : cs) = shift c (negate l) : deCaesar l cs

vigenere' :: IO [Char]
vigenere' = do
  hSetBuffering stdout NoBuffering
  putStr "Please input key: "
  key <- getLine
  putStr "Please input text: "
  vigenere key <$> getLine

caesar' :: IO [Char]
caesar' = do
  hSetBuffering stdout NoBuffering
  putStr "Please input key: "
  key <- getLine
  case key of
    [c] -> do
      putStr "Please input text: "
      caesar (digitToInt c) <$> getLine
    _ -> return "key must be a single character"
