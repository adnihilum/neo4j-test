{-# LANGUAGE OverloadedStrings #-}

module NeoTest
  ( run
  ) where

import Control.Monad.IO.Class
import Data.Default
import qualified Data.Text as T
import qualified Database.Bolt as B
import Database.Bolt (BoltActionT)

run :: (MonadIO m) => m ()
run = do
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

action :: (MonadIO m) => BoltActionT m ()
action = do
  B.query "CREATE (p:Person)-[:LIKES]->(t:Technology)" >>= liftIO . print
  B.query "MATCH (p:Person)-[:LIKES]-(t:Technology) RETURN p, t" >>=
    liftIO . print
  return ()
