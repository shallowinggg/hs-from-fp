module Applicative.List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f lf) (Cons a la) = Cons (f a) (g (f <$> la) (g (lf <*> pure a) (lf <*> la)))
    where
      g Nil x = x
      g (Cons a Nil) x = Cons a x
      g (Cons a ls) x = Cons a (g ls x)
