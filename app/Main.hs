module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Default
import qualified Data.Text as T
import Database
import qualified Database.Bolt as B
import Database.Bolt (BoltActionT)
import Domain
import IOHelpers
import Populate

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
  catchIOError $ populate
  return ()
