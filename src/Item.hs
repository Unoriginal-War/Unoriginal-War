{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
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
    , buildingDescription
    , buildingPosition
    , buildingSize
    , buildingTexture
    , rPosition
    , rSize
    , rTexture
    , unitDescription
    , unitPlan
    , unitPosition
    , unitSize
    , unitSpeed
    , unitTexture
    )
where

import Control.Lens
import Data.Hashable
import Data.Word
import GHC.Generics
import Linear
import SDL


type Position = V2 Float
type Size = Word8
type Speed = Float

data UnitDescription = UnitDescription
    { _unitSpeed :: Speed
    , _unitSize :: Size
    , _unitTexture :: FilePath
    }
  deriving (Eq, Generic)

instance Hashable UnitDescription

data Unit = Unit
    { _unitPosition :: Position
    , _unitPlan :: [Action]
    , _unitDescription :: UnitDescription
    }

data BuildingDescription = BuildingDescription
    { _buildingSize :: Size
    , _buildingTexture :: FilePath
    }
  deriving (Eq, Generic)

instance Hashable BuildingDescription

data Building = Building
    { _buildingPosition :: Position
    , _buildingDescription :: BuildingDescription
    }

data RenderingInfo = RenderingInfo
    { _rPosition :: Position
    , _rSize :: Size
    , _rTexture :: Texture
    }

data Action = MoveTo Position
  deriving (Eq)

makeLenses ''Unit
makeLenses ''UnitDescription
makeLenses ''Building
makeLenses ''BuildingDescription
makeLenses ''RenderingInfo
