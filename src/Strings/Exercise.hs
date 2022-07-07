module Strings.Exercise where

-- p5

-- only work for "Curry is awesome"
rvrs :: String -> String
rvrs s = drop 9 s ++ " " ++ drop 6 (take 8 s) ++ " " ++ take 5 s