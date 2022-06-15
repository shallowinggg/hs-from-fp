-- p1

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

-- p2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') = v1 == v1' && v2 == v2'

-- p3

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString v) (TisAString v') = v == v'
  (==) _ _ = False

-- p4

data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair v1 v2) (Pair v1' v2') = v1 == v1' && v2 == v2'

-- p5

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v1 v2) (Tuple v1' v2') = v1 == v1' && v2 == v2'

-- p6

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'
  (==) _ _ = False

-- p7

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'
  (==) _ _ = False
