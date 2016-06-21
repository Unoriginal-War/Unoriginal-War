module Types
    ( Action(..)
    , Building(..)
    , DTime
    , Input(..)
    , Position
    , RenderingInfo(..)
    , State(..)
    , StaticInfo(..)
    , Unit(..)
    )
where

import Data.Map.Strict
import Data.Time.Clock
import Data.Vector
import Data.Word
import Linear
import SDL.Input (Scancode)


type Position = V2 Float
type Color = V4 Word8
type Size = Word8
type Speed = Float

data StaticInfo = StaticInfo
    { staticColor :: Color
    , staticSize :: Size
    , staticSpeed :: Speed
    }

data RenderingInfo = RenderingInfo
    { position :: Position
    , color :: Color
    , size :: Size
    }

data Action = MoveTo Position

data Unit = Unit
    { unitPosition :: Position
    , unitStatic :: StaticInfo
    , unitPlan :: [Action]
    }

data Building = Building
    { buildingPosition :: Position
    , buildingStatic :: StaticInfo
    }

data State = State
    { units :: Vector Unit
    , buildings :: Vector Building
    }

data Input = Input
    { keyboard :: Map Scancode ()
    }

type DTime = NominalDiffTime
