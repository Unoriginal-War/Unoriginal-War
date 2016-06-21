{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Item
    ( Sources(..)
    , Features(..)
    , Resources(..)
    , Position
    , Size
    , Description(..)
    , Speed
    , Item(..)
    , ItemType(..)
    , Properties(..)
    , RenderingInfo(..)
    , State(..)
    , Action(..)
    , Unit
    , Building
    )
where

import Data.Vector
import Data.Word
import Linear
import SDL


type Position = V2 Float
type Size = Word8
type Speed = Float

data Sources (a :: ItemType) = Sources
    { textureFile :: FilePath
    }

data family Features (a :: ItemType)
data instance Features 'UnitType = UnitFeatures
    { speed :: Speed
    , size :: Size
    }
data instance Features 'BuildingType = BuildingFeatures
    { bSize :: Size
    }

-- Handle loading stages to get rid of the Maybe.
data Resources (a :: ItemType) = Resources
    { texture :: Maybe Texture
    }

data Description (a :: ItemType) = Description
    { features :: Features a
    , sources :: Sources a
    }

data family Properties (a :: ItemType)
data instance Properties 'UnitType = UnitProperties
    { position :: Position
    , plan :: [Action]
    }
data instance Properties 'BuildingType = BuildingProperties
    { bPosition :: Position
    }

data ItemType = UnitType | BuildingType


data Item (a :: ItemType) = Item
    { description :: Description a
    , resources :: Resources a
    , properties :: Properties a
    }
type Unit = Item 'UnitType
type Building = Item 'BuildingType

data RenderingInfo = RenderingInfo
    { rPosition :: Position
    , rSize :: Size
    , rTexture :: Texture
    }


data Action = MoveTo Position

data State = State
    { units :: Vector Unit
    , buildings :: Vector Building
    }

