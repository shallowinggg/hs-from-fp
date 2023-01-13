module State.RollYourOwn where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import GHC.Base (liftA3)
import System.Random (Random (randomR), StdGen)

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly.
    x ->
      error $
        "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
           in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = go 0 0
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go sum count limit gen
      | sum >= limit = count
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
           in go (sum + die) (count + 1) limit nextGen

rollsCountLogged ::
  Int ->
  StdGen ->
  (Int, [Die])
rollsCountLogged = go 0 0 []
  where
    go :: Int -> Int -> [Die] -> Int -> StdGen -> (Int, [Die])
    go sum count logs limit gen
      | sum >= limit = (count, logs)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
           in go (sum + die) (count + 1) (intToDie die : logs) limit nextGen
