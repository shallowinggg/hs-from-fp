module Foldable.LibraryFunctions where

-- 1.

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- 2.

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

-- 3.

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> a == x || b) False

-- 4.

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =
  foldr
    ( \a b -> case b of
        Nothing -> Just a
        Just x -> if a < x then Just a else b
    )
    Nothing

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' =
  foldr
    ( \a b -> case b of
        Nothing -> Just a
        Just x -> if a > x then Just a else b
    )
    Nothing

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = foldr (\a b -> False) True

-- 7.
length' :: (Foldable t) => t a -> Int
length' = foldr (\a b -> b + 1) 0

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap' id

-- 10.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty
