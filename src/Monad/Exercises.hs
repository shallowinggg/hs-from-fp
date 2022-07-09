module Monad.Exercises where

import GHC.Base (join, liftA, liftA2)
import Test.QuickCheck (Arbitrary, elements)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Checkers (EqProp, eq, (=-=))

-- 1.

data Nope a = NopeDotJpg
  deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- 2.

data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Right' b <*> _ = Right' b
  Left' f <*> Right' b = Right' b
  Left' f <*> Left' a = Left' $ f a

instance Monad (PhhhbbtttEither b) where
  return = pure
  Right' b >>= _ = Right' b
  Left' a >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [Left' a, Right' b]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- 3.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 4.

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

append :: List a -> List a -> List a
append Nil x = x
append (Cons a ls) x = Cons a (append ls x)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  -- todo: fix
  (<*>) (Cons f lf) (Cons a la) = Cons (f a) (append (f <$> la) (append (lf <*> pure a) (lf <*> la)))

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a l) >>= f = append (f a) (l >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    elements [Nil, Cons a as]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- 1.

j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = (:) <$> f x <*> meh xs f

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs (join . return)
