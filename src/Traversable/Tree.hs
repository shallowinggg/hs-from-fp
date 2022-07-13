module Traversable.Tree where

import Test.QuickCheck (Arbitrary (arbitrary), arbitrary, elements)
import Test.QuickCheck.Checkers (EqProp (..), eq)

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (f <$> t1) (f a) (f <$> t2)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    t1 <- arbitrary
    t2 <- arbitrary
    elements [Empty, Leaf a, Node t1 a t2]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq
