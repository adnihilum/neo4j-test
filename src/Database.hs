module Database where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Database.Bolt as B
import Database.Bolt (BoltActionT)
import Domain

-- Neo4j db stuff
setConstrains :: (MonadIO m) => BoltActionT m ()
setConstrains = do
  B.query_ "CREATE CONSTRAINT ON (x:Reaction) ASSERT x.id IS UNIQUE"
  B.query_ "CREATE CONSTRAINT ON (x:Molecule) ASSERT x.id IS UNIQUE"
  B.query_ "CREATE CONSTRAINT ON (x:Catalyst) ASSERT x.id IS UNIQUE"

-- create nodes
createReaction :: (MonadIO m) => Reaction -> BoltActionT m ()
createReaction reaction =
  B.queryP_ "create (r:Reaction) set r = $reaction" $
  M.fromList
    [ ( "reaction"
      , B.M $
        M.fromList
          [ ("id", B.I $ reactionId reaction)
          , ("name", B.T $ T.pack $ reactionName reaction)
          ])
    ]

createMolecule :: (MonadIO m) => Molecule -> BoltActionT m ()
createMolecule molecule =
  B.queryP_ "create (o: Molecule) set o = $object" $
  M.fromList
    [ ( "object"
      , B.M $
        M.fromList
          [ ("id", B.I $ moleculeId molecule)
          , ("smiles", B.T $ T.pack $ moleculeSmiles molecule)
          , ("iupacName", B.T $ T.pack $ moleculeIupacName molecule)
          ])
    ]

createCatalyst :: (MonadIO m) => Catalyst -> BoltActionT m ()
createCatalyst catalyst =
  B.queryP_ "create (o: Catalyst) set o = $object" $
  M.fromList
    [ ( "object"
      , B.M $
        M.fromList $
        [ ("id", B.I $ catalystId catalyst)
        , ("smiles", B.T $ T.pack $ catalystSmiles catalyst)
        ] ++
        maybeToList ((,) "name" . B.T . T.pack <$> catalystName catalyst))
    ]
