module MonadRandom where

import Control.Monad.State
import System.Random

type RandomM = State StdGen

randomR' :: (Random a) => (a, a) -> RandomM a
randomR' range = do
  g <- get
  let (ret, g') = randomR range g
  put g'
  return ret

runRandomM :: Int -> RandomM a -> a
runRandomM x a = evalState a gen
  where
    gen = mkStdGen x
