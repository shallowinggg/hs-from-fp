module Signaling.Exercises where

import Data.List (intercalate)

-- Determine the kinds

-- 1. a :: *
-- 2.
--  a :: *
--  f :: * -> *

-- String processing

-- 1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe s =
  intercalate " " $
    map
      ( \w -> case notThe w of
          Nothing -> "a"
          Just s -> s
      )
      $ words s

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel s = go ws
  where
    go [] = 0
    go [x] = 0
    go ("the" : all@(x : xs)) = if startWithVowel x then 1 + go xs else go all
    go (x : xs) = go xs
    ws = words s

startWithVowel :: [Char] -> Bool
startWithVowel "" = False
startWithVowel (c : cs) = c `elem` "aeiou"

-- 3.
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

-- Validate the word

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  if vowels > consonants
    then Nothing
    else Just (Word' s)
  where
    vowels = length $ filter isVowel s
    consonants = length s - vowels

-- It’s only Natural

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n < 0 = Nothing
  | otherwise =
    Just
      ( Succ
          ( case integerToNat (n - 1) of
              Just Zero -> Zero
              Just x -> x
              Nothing -> error "never happen"
          )
      )

-- Small library for Maybe

-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee init _ Nothing = init
mayybee _ f (Just x) = f x

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe init Nothing = init
fromMaybe _ (Just x) = x

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x : xs) = case x of
  Just x -> x : catMaybes xs
  Nothing -> catMaybes xs

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (x : xs) = case x of
  Nothing -> Nothing
  Just x -> case flipMaybe xs of
    Nothing -> Nothing
    Just xs -> Just (x : xs)

-- Small library for Either

-- 1.
lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    ( \x ys -> case x of
        Left x -> x : ys
        Right _ -> ys
    )
    []

-- 2.
rights' :: [Either a b] -> [b]
rights' =
  foldr
    ( \x ys -> case x of
        Left _ -> ys
        Right x -> x : ys
    )
    []

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
  foldr
    ( \x (xs, ys) -> case x of
        Left v -> (v : xs, ys)
        Right v -> (xs, v : ys)
    )
    ([], [])

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f x = case x of
  Left _ -> Nothing
  Right x -> Just (f x)

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g x = case x of
  Left a -> f a
  Right b -> g b

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Write your own iterate and unfoldr

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f init = init : myIterate f (f init)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (a, b) -> a : myUnfoldr f b
  Nothing -> []

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Finally something other than a list!

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Just (x, y, z) -> Node (unfold f x) y (unfold f z)
  Nothing -> Leaf

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    ( \(x, y) ->
        if x == 0
          then Nothing
          else Just ((x - 1, y + 1), y, (x - 1, y + 1))
    )
    (n, 0)
