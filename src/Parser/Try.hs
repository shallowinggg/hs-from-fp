module Parser.Try where

import Data.Ratio ((%))
import GHC.Base (Alternative ((<|>)))
import Text.Trifecta (CharParsing (char), Parser, Parsing (try), decimal)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type IntegerOrRational = Either Rational Integer

parseIor :: Parser (Either Rational Integer)
parseIor = try (Left <$> parseFraction) <|> try (Right <$> decimal)