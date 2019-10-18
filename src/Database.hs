module Database where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Foldable (fold)
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Database.Bolt as B
import Database.Bolt (BoltActionT)
import Domain

-- Neo4j db stuff
deleteAllNodes :: (MonadIO m) => BoltActionT m ()
deleteAllNodes = B.query_ "match (a) detach delete a"

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
createMolecule molecule = do
  (liftIO $ print $ molecule)
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

-- create links  for a reaction
linkReaction ::
     (MonadIO m)
  => (Int, Int, Int, Int, Int, ACCELERATE, PRODUCT_FROM)
  -> BoltActionT m ()
linkReaction (reactionId, reagentAId, reagentBId, resultId, catalystId, accelerate, productFrom) = do
  let queryList =
        [ "match (reaction:Reaction{id: $reactionId})"
        , "match (reagentA:Molecule{id: $reagentAId})"
        , "match (reagentB:Molecule{id: $reagentBId})"
        , "match (result:Molecule{id: $resultId})"
        , "match (catalyst:Catalyst{id: $catalystId})"
        , "create (catalyst)-[accelerate:ACCELERATE]->(reaction) set accelerate = $accelerate"
        , "create (reagentA)-[rA:REAGENT_IN]->(reaction)"
        , "create (reagentB)-[rB:REAGENT_IN]->(reaction)"
        , "create (reaction)-[productFrom:PRODUCT_FROM]->(result) set productFrom = $productFrom"
        ]
  let query = fold $ intersperse "\n" queryList
  let queryParams =
        M.fromList
          [ ("reactionId", B.I reactionId)
          , ("reagentAId", B.I reagentAId)
          , ("reagentBId", B.I reagentBId)
          , ("resultId", B.I resultId)
          , ("catalystId", B.I catalystId)
          , ( "accelerate"
            , B.M $
              M.fromList
                [ ( "temperature"
                  , B.F $ realToFrac $ accelerateTemperature accelerate)
                , ("pressure", B.F $ realToFrac $ acceleratePressure accelerate)
                ])
          , ( "productFrom"
            , B.M $
              M.fromList
                [("ammount", B.F $ realToFrac $ productFromAmount productFrom)])
          ]
  B.queryP_ query queryParams
