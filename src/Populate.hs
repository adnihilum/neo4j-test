module Populate
  ( populate
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Database
import Database.Bolt (BoltActionT)
import Domain
import MonadRandom

--TODO: generate complete reactions with reagents, catalyst, and links
populate :: (MonadIO m) => BoltActionT m ()
populate = do
  populateObjects
  populateLinkedReactions

--1. population objects
populateObjects :: (MonadIO m) => BoltActionT m ()
populateObjects = do
  let ids = [fstId .. lastId]
  let molecules = genMolecule <$> ids
  let reactions = genReaction <$> ids
  let catalysts = genCatalyst <$> ids
  mapM_ createMolecule molecules

idRange :: (Int, Int)
idRange = (1, 20)

fstId :: Int
fstId = fst idRange

lastId :: Int
lastId = snd idRange

--2. population fully linked reactions
populateLinkedReactions :: (MonadIO m) => BoltActionT m ()
populateLinkedReactions = do
  let seed = 101
  let genReactions = genRandomLinkedReactions `mapM` [fstId .. lastId]
  let reactions = linkReaction <$> runRandomM seed genReactions
  sequence_ reactions

genRandomLinkedReactions ::
     Int -> RandomM (Int, Int, Int, Int, ACCELERATE, PRODUCT_FROM)
genRandomLinkedReactions reactionId = do
  reagentA <- randomR' idRange
  reagentB <- randomR' idRange
  result <- randomR' idRange
  accelerate <- accelerateR
  productFrom <- productFromR
  return (reactionId, reagentA, reagentB, result, accelerate, productFrom)
  where
    accelerateR = do
      pressure <- randomR' (1.0, 100.0)
      temperature <- randomR' (1.0, 200.0)
      return
        ACCELERATE
          {acceleratePressure = pressure, accelerateTemperature = temperature}
    productFromR = do
      ammount <- randomR' (10.0, 20.0)
      return PRODUCT_FROM {productFromAmount = ammount}

-- generate objects
genMolecule :: Int -> Molecule
genMolecule id =
  Molecule
    { moleculeId = id
    , moleculeIupacName = "iupackName" <> strId
    , moleculeSmiles = "smiles" <> strId
    }
  where
    strId = show id

genReaction :: Int -> Reaction
genReaction id =
  Reaction {reactionId = id, reactionName = "reaction" <> show id}

genCatalyst :: Int -> Catalyst
genCatalyst id =
  Catalyst
    { catalystId = id
    , catalystSmiles = "catalistSmiles" <> show id
    , catalystName = Just $ "catalystName" <> show id
    }
