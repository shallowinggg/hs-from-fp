module Lists.PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

myLines :: String -> [String]
myLines "" = []
myLines x = takeWhile (/= '\n') y : myLines (dropWhile (/= '\n') y)
  where
    y = dropWhile (== '\n') x

-- What we want 'myLines sentences' -- to equal
shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test -- to ensure you've written your function -- correctly.
poemLines :: IO ()
poemLines =
  print $
    "Are they equal? "
      ++ show (myLines sentences == shouldEqual)
