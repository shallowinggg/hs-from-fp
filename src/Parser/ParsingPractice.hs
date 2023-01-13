module Parser.ParsingPractice where

import Text.Trifecta (CharParsing (char, string), Parser, Parsing (eof), parseString)

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

-- 1.
one :: Parser ()
one = char '1' >> eof

oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

-- 2.

strOne :: Parser String
strOne = string "1"

strOneTwo :: Parser String
strOneTwo = string "12"

strOneTwoThree :: Parser String
strOneTwoThree = string "123"

-- 3.
string' :: CharParsing m => String -> m String
string' = traverse char
