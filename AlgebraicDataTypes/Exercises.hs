module AlgebraicDataTypes.Exercises where

import Text.XHtml (menu)

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

-- Exercises: Dog Types

data Doggies a = Husky a | Mastiff a
  deriving (Eq, Show)

-- 1. type constructor
-- 2. Doggies :: * -> *
-- 3. Doggies String :: String -> *
-- 4. Husky 10 :: Num a => Doggies a
-- 5. Husky (10 :: Integer) :: Doggies Integer
-- 6. Mastiff "Scooby Doo" :: Doggies String
-- 7. both, it depends
-- 8. DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux "doggie!" :: DogueDeBordeaux String

--
-- Exercises: Cardinality

-- 1. 1
-- 2. 3
-- 3. 65536
-- 4. Int 18446744073709551616
--    Integer infinite
-- 5. 2 ^ 8

--
-- Exercises: For Example

-- 1.
-- MakeExample :: Example
-- :t Example :: error

-- 2.
-- type Example :: *
-- data Example = MakeExample
--      instance [safe] Show Example
-- Show

-- 3.
-- MakeExample' :: Int -> Example'
data Example' = MakeExample' Int

--
-- Exercises: Pity the Bool

-- 1. 4

-- 2.
-- a) 258
-- b) overflow warning
-- c) overflow warning

--
-- Exercises: How Does Your Garden Grow?

-- 1.
data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden
  = Garden Gardener FlowerType
  deriving (Show)

data Garden'
  = Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Lilac' Gardener
  deriving (Show)

-- 11.13 Constructing and deconstructing values

data GuessWhat
  = Chickenbutt
  deriving (Eq, Show)

data Id a
  = MkId a
  deriving (Eq, Show)

data Product a b
  = Product a b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a,
    psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow = NumCow Int
  deriving (Eq, Show)

newtype NumPig = NumPig Int
  deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse
  = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo
  = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo
  = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- Alternately
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

--
-- Multiple choice

-- 1. a)
-- 2. c)
-- 3. b)
-- 4. c)
