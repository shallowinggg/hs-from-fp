module AlgebraicDataTypes.AsPatterns where

import Data.Char (toUpper)

-- 1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xall@(x : xs) (y : ys) = (x == y && isSubseqOf xs ys) || isSubseqOf xall ys

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\word@(c : cs) -> (word, toUpper c : cs)) . words
