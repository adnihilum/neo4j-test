module Domain where

data Molecule =
  Molecule
    { moleculeId :: Int
    , moleculeSmiles :: String
    , moleculeIupacName :: String
    }
  deriving (Show)

data Reaction =
  Reaction
    { reactionId :: Int
    , reactionName :: String
    }

data Catalyst =
  Catalyst
    { catalystId :: Int
    , catalystSmiles :: String
    , catalystName :: Maybe String
    }

data PRODUCT_FROM =
  PRODUCT_FROM
    { productFromAmount :: Float
    }

data ACCELERATE =
  ACCELERATE
    { accelerateTemperature :: Float
    , acceleratePressure :: Float
    }
