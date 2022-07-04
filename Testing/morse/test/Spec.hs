module Main where

import ArithTest (qcHalf)
import qualified Data.Map as M
import Morse (Morse, charToMorse, letterToMorse, morseToChar)
import Test.QuickCheck (Gen, Property, elements, forAll, quickCheck)
import WordNumberTest (testWordNumber)

main :: IO ()
main = do
  -- quickCheck prop_thereAndBackAgain
  -- testWordNumber
  qcHalf

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
