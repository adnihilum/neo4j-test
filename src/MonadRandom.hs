module MonadRandom where

import Control.Monad.State
import System.Random

type MonadRandomT m = StateT StdGen m

class (MonadState StdGen m) =>
      MonadRandom m
  where
  randomR' :: (Random a) => (a, a) -> m a

instance (Monad m) => MonadRandom (StateT StdGen m) where
  randomR' range = do
    g <- get
    let (ret, g') = randomR range g
    put g'
    return ret

runGenerator :: (Monad m) => Int -> MonadRandomT m a -> m a
runGenerator x ma = fst <$> runStateT ma gen
  where
    gen = mkStdGen x
