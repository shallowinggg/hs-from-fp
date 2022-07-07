module AlgebraicDataTypes.Phone where

import Data.Char (isUpper, toUpper)
import Data.List (elemIndex, group, groupBy, maximumBy, sort, sortBy)

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

validDigits = ['0' .. '9'] ++ ['*', '#']

validLetters = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "^+_.,"

-- 1.
data Button = Button Digit String
  deriving (Show)

tapOnButton :: Button -> Char -> [(Digit, Presses)]
tapOnButton (Button digit cs) c
  | c `notElem` validLetters = []
  | digit == c = [(digit, length cs + 1)]
  | isUpper c = case elemIndex c cs of
    Nothing -> []
    Just i -> [('*', 1), (digit, i + 1)]
  | otherwise = case elemIndex (toUpper c) cs of
    Nothing -> []
    Just i -> [(digit, i + 1)]

newtype DaPhone = DaPhone [Button]
  deriving (Show)

phone :: DaPhone
phone =
  DaPhone
    [ Button '1' "",
      Button '2' "ABC",
      Button '3' "DEF",
      Button '4' "GHI",
      Button '5' "JKL",
      Button '6' "MNO",
      Button '7' "PQRS",
      Button '8' "TUV",
      Button '9' "WXYZ",
      Button '*' "^",
      Button '0' "+_",
      Button '#' ".,"
    ]

-- 2.
convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps ::
  DaPhone ->
  Char ->
  [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps (DaPhone (x : xs)) c = if not (null res) then res else reverseTaps (DaPhone xs) c
  where
    res = tapOnButton x c

cellPhonesDead ::
  DaPhone ->
  String ->
  [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, n) y -> n + y) 0

-- 4.
mostPopularLetter :: String -> Char
mostPopularLetter =
  fst
    . maximumBy (\(_, n1) (_, n2) -> compare n1 n2)
    . map (\xs -> (head xs, length xs))
    . group
    . sort
    . filter (`elem` validLetters)

mostPopularDigit :: String -> Char
mostPopularDigit = fst . mostPopularLetterCost

mostPopularLetterCost :: String -> (Digit, Presses)
mostPopularLetterCost s =
  maximumBy (\(_, n1) (_, n2) -> compare n1 n2) $
    map (\xs@((d, _) : _) -> (d, fingerTaps xs)) $
      groupBy (\(d1, _) (d2, _) -> d1 == d2) $
        cellPhonesDead phone s

-- 5.
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord =
  fst
    . maximumBy (\(_, n1) (_, n2) -> compare n1 n2)
    . map (\xs -> (head xs, length xs))
    . group
    . words
    . concat
