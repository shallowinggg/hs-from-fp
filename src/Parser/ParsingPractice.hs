module Parser.ParsingPractice where

import Text.Trifecta (CharParsing (char), Parser, Parsing (eof), parseString)

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

-- 1.
one :: Parser ()
one = char '1' >> eof

oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

-- 2.

-- 3.
string' :: CharParsing m => String -> m String
string' = traverse char
