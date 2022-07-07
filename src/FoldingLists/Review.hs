module FoldingLists.Review where

-- 1.
stops = "pbtdkg"

vowels = "aeiou"

-- a)
p1 = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- b)
p2 = [('p', y, z) | y <- vowels, z <- stops]

-- c)
nouns = ["he", "dog", "restaurant", "cat"]

verbs = ["run", "call", "type"]

p3 = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2.
-- avg word length of the given sentence
seekritFunc :: String -> Int
seekritFunc x =
  div
    (sum (map length (words x)))
    (length (words x))

-- 3.
seekritFunc' :: Fractional a => String -> a
seekritFunc' x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
