module Populate where

import Domain

--TODO: generate complete reactions with reagents, catalyst, and links

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

genCatalist :: Int -> Catalyst
genCatalist id =
  Catalyst
    { catalystId = id
    , catalystSmiles = "catalistSmiles" <> show id
    , catalystName = Just $ "catalystName" <> show id
    }
