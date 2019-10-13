module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Default
import qualified Data.Text as T
import Database
import qualified Database.Bolt as B
import Database.Bolt (BoltActionT)
import Domain

main :: (Show e, MonadError e m, MonadIO m) => m ()
main = do
  let config =
        def
          { B.host = "localhost"
          , B.port = 7687
          , B.user = "neo4j"
          , B.password = "test"
          }
  pipe <- B.connect config
  B.run pipe action
  B.close pipe
  return ()

action :: (Show e, MonadError e m, MonadIO m) => BoltActionT m ()
action = do
  setConstrains
  catchIOError $
    createReaction Reaction {reactionId = 101, reactionName = "ssss"}
  catchIOError
    $createMolecule
    Molecule
      { moleculeId = 201
      , moleculeIupacName = "some molecule iupack"
      , moleculeSmiles = "some molecule smiles"
      }
  catchIOError
    $createCatalyst
    Catalyst
      { catalystId = 301
      , catalystSmiles = "some catalyst smiles"
      , catalystName = Just "awesome catalyst"
      }
  return ()

-- IO helpers 
print_ :: (MonadIO m, Show a) => a -> m ()
print_ = liftIO . print

catchIOError ::
     forall e m. (MonadError e m, MonadIO m, Show e)
  => m ()
  -> m ()
catchIOError ma = catchError ma printError
  where
    printError :: e -> m ()
    printError err = liftIO (putStr "IO exception: " >> print err)
