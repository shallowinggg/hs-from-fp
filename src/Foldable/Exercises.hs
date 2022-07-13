module Foldable.Exercises where

-- 1.
newtype Constant a b = Constant b
  deriving (Show, Eq)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2.
data Two a b = Two a b
  deriving (Show, Eq)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

-- 3.

data Three a b c = Three a b c
  deriving (Show, Eq)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- 4.
data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 <> f b2

-- 5.
data Four' a b = Four' a b b b
  deriving (Show, Eq)

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

-- 6.

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
