{-# LANGUAGE InstanceSigs #-}

module Reader.ReadingComprehension where

-- 1.
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2.
newtype Reader r a = Reader {runReader :: r -> a}

asks :: (r -> a) -> Reader r a
asks = Reader

-- 3.
instance Functor (Reader r) where
  fmap f (Reader ra) =
    Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader fab <*> Reader ra = Reader $ \r -> fab r (ra r)

-- Exercise: Reader Monad

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

--

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addr <- Reader address
  return $ Dog name addr
