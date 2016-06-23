{-# LANGUAGE DeriveGeneric #-}
module Item
    ( Building(..)
    , BuildingDescription(..)
    , Position
    , Size
    , Speed
    , RenderingInfo(..)
    , Action(..)
    , Unit(..)
    , UnitDescription(..)
    )
where

import Data.Hashable
import Data.Word
import GHC.Generics
import Linear
import SDL


type Position = V2 Float
type Size = Word8
type Speed = Float

data UnitDescription = UnitDescription
    { unitSpeed :: Speed
    , unitSize :: Size
    , unitTexture :: FilePath
    }
  deriving (Eq, Generic)

instance Hashable UnitDescription

data Unit = Unit
    { unitPosition :: Position
    , unitPlan :: [Action]
    , unitDescription :: UnitDescription
    }

data BuildingDescription = BuildingDescription
    { buildingSize :: Size
    , buildingTexture :: FilePath
    }
  deriving (Eq, Generic)

instance Hashable BuildingDescription

data Building = Building
    { buildingPosition :: Position
    , buildingDescription :: BuildingDescription
    }

data RenderingInfo = RenderingInfo
    { rPosition :: Position
    , rSize :: Size
    , rTexture :: Texture
    }

data Action = MoveTo Position
  deriving (Eq)
