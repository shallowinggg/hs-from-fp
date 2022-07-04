module Monoid.Optional where

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Nada <> x = x
  x <> Nada = x
  (Only x) <> (Only y) = Only $ x <> y

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada

  mappend x Nada = x
  mappend Nada x = x
  mappend (Only x) (Only y) = Only $ mappend x y
