module Testing.FunctorTest where

import Test.QuickCheck (quickCheck)

functorId :: (Eq (f b), Functor f) => f b -> Bool
functorId x = fmap id x == x

qcFunctorId :: IO ()
qcFunctorId = do
  quickCheck (functorId :: [Int] -> Bool)

functorCompose :: (Eq (f b1), Functor f) => (a -> b2) -> (b2 -> b1) -> f a -> Bool
functorCompose f g x =
  fmap (g . f) x == fmap g (fmap f x)

qcFunctorCompose :: IO ()
qcFunctorCompose = do
  quickCheck (functorCompose (+ 1) (* 2) :: [Int] -> Bool)
