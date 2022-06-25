module AlgebraicDataTypes.Vehicle where

-- Exercises: Vehicles

data Price = Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

-- 1. myCar :: Vehicle
-- 2.

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

-- 4.
-- throw exception

-- 5.
data Size = Size Integer
  deriving (Eq, Show)

data Vehicle'
  = Car' Manufacturer Price
  | Plane' Airline Size
  deriving (Eq, Show)

isCar' :: Vehicle' -> Bool
isCar' (Car' _ _) = True
isCar' _ = False

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' _ = False
