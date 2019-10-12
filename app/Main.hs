module Main where
    
import Control.Monad.IO.Class
import Data.Default
import qualified Data.Text as T
import qualified Database.Bolt as B
import Database.Bolt (BoltActionT)

main :: (MonadIO m) => m ()
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

action :: (MonadIO m) => BoltActionT m ()
action = do
  B.query "CREATE (p:Person)-[:LIKES]->(t:Technology)" >>= print_
  B.query "MATCH (p:Person)-[:LIKES]-(t:Technology) RETURN p, t" >>= print_
  return ()
  where
    print_ = liftIO . print
