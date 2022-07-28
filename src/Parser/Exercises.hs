module Parser.Exercises where

import GHC.Base (Alternative (many, (<|>)))
import Text.Trifecta (Parser, char, digit, integer, letter, option, some)

-- 1.
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  NOSS s1 <= NOSS s2 = s1 <= s2
  NOSI n1 <= NOSI n2 = n1 <= n2
  NOSI _ <= NOSS _ = True
  _ <= _ = False

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  SemVer major minor patch release _ <= SemVer major' minor' patch' release' _
    | major < major' = True
    | minor > major' = False
    | minor < minor' = True
    | minor > minor' = False
    | patch < patch' = True
    | patch > patch' = False
    | null release' = True
    | null release = False
    | otherwise = release <= release'

parseSemVer :: Parser SemVer
parseSemVer = do
  major' <- major
  _ <- char '.'
  minor' <- minor
  _ <- char '.'
  patch' <- patch
  release' <- release
  SemVer major' minor' patch' release' <$> metadata

int :: Parser Integer
int = do
  digits <- some digit
  -- leading zero
  if length digits >= 2 && head digits == '0'
    then fail "Leading zero"
    else return $ read digits

major :: Parser Integer
major = int

minor :: Parser Integer
minor = int

patch :: Parser Integer
patch = int

identifier :: Parser NumberOrString
identifier =
  NOSI <$> int
    <|> NOSS <$> some (letter <|> digit <|> char '-')

leadingZeroDigit :: Parser Integer
leadingZeroDigit = do
  digits <- some digit
  return $ read digits

metaId :: Parser NumberOrString
metaId =
  NOSI <$> leadingZeroDigit
    <|> NOSS <$> some (letter <|> digit <|> char '-')

ids :: Char -> Parser NumberOrString -> Parser [NumberOrString]
ids separator id = do
  _ <- char separator
  fst <- id
  others <- many (char '.' >> id)
  return $ fst : others

release :: Parser [NumberOrString]
release = option [] (ids '-' identifier)

metadata :: Parser [NumberOrString]
metadata = option [] (ids '+' metaId)

-- 2.
