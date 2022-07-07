module Applicative.Constant where

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant f) (Constant b) = Constant (f <> b)
