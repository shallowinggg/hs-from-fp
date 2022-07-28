module Parser.ParsingFractions where

import Text.Trifecta (Parser, Parsing (eof), integer')

yourFuncHere :: Parser Integer
yourFuncHere = integer' <* eof
