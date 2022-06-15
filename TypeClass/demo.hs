module TypeClass.Demo where

-- Eq

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date
  = Date DayOfWeek Int

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') =
      weekday == weekday'
        && dayOfMonth == dayOfMonth'

data Identity a
  = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Num

-- Type-defaulting TypeClasses

-- default Num Integer
-- default Real Integer
-- default Enum Integer
-- default Integral Integer
-- default Fractional Double
-- default RealFrac Double
-- default Floating Double
-- default RealFloat Double

-- Ord

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

data OrdDemo = OrdDemo1 | OrdDemo2
  deriving (Eq, Ord, Show)

-- Enum

data EnumDemo = Enum1 | Enum2 | Enum3 | Enum4 | Enum5
  deriving (Enum, Show)

-- Show

data ShowDemo = ShowDemo
  deriving (Show)
