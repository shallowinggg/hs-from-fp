{-# LANGUAGE ScopedTypeVariables #-}

module FoldingLists.Database where

import Data.Time
  ( UTCTime (UTCTime),
    fromGregorian,
    secondsToDiffTime,
  )

-- Exercises: Database Processing

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbNumber 8000,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

-- 1.
f x = case x of
  DbDate utc -> [utc]
  _ -> []

filterDbDate :: Foldable t => t DatabaseItem -> [UTCTime]
filterDbDate = foldr (\x y -> f x ++ y) []

-- 2.
f2 x = case x of
  DbNumber num -> [num]
  _ -> []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\x y -> f2 x ++ y) []

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb [] = 0
avgDb xs = fromIntegral total / fromIntegral num
  where
    nums = filterDbNumber xs
    total = sum nums
    num = length nums
