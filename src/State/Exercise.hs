{-# LANGUAGE TupleSections #-}

module State.Exercise where

import qualified Data.Bifunctor

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State sa) = State $ \s -> let r = sa s in Data.Bifunctor.first f r

instance Applicative (State s) where
  pure a = State (a,)
  State f <*> State sa = State $
    \s ->
      let (a, s') = sa s
          (g, s'') = f s
       in (g a, s')

instance Monad (State s) where
  return = pure
  State sa >>= f = State $ \s ->
    let (a, s') = sa s
        sb = f a
     in runState sb s'

-- 1.
get :: State s s
get = State $ \s -> (s, s)

-- 2.
put :: s -> State s ()
put s = State $ const ((), s)

-- 3.
exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

-- 4.
eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s

-- 5.
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
