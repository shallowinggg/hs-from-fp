module Testing.MorseTest where

import qualified Data.Map as M
import Test.QuickCheck (Gen, Property, elements, forAll, quickCheck)
import Testing.Morse (Morse, charToMorse, letterToMorse, morseToChar)

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

qcThereAndBackAgain :: IO ()
qcThereAndBackAgain = quickCheck prop_thereAndBackAgain
