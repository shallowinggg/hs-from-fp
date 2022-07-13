module Traversable.Instances where

import GHC.Base (Applicative (liftA2), liftA3)
import Test.QuickCheck (Arbitrary (arbitrary), Property, Testable (property), elements, (.&.))
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

-- 1.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 2.

newtype Constant a b = Constant {getConstant :: a}
  deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

-- 3.
data Optional a = Nada | Yep a
  deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

-- 4.
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (f <$> as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    elements
      [ Nil,
        Cons a as
      ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- 5.
data Three a b c = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 6.
data Pair a b = Pair a b
  deriving (Show, Eq)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- 7.
data Big a b = Big a b b
  deriving (Show, Eq)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big a b1 b2) = f b1 <> f b2

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- 8.
data Bigger a b = Bigger a b b b
  deriving (Show, Eq)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b1 b2 b3) = f b1 <> f b2 <> f b3

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 9.
data S n a = S (n a) a
  deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance
  ( Functor n,
    Arbitrary (n a),
    Arbitrary a
  ) =>
  Arbitrary (S n a)
  where
  arbitrary = S <$> arbitrary <*> arbitrary

instance
  ( Applicative n,
    Testable (n Property),
    EqProp a
  ) =>
  EqProp (S n a)
  where
  (S x y) =-= (S p q) =
    property ((=-=) <$> x <*> p)
      .&. (y =-= q)
