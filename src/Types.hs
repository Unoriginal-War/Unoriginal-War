module Types
    ( Building(..)
    , DTime
    , Input(..)
    , RenderingInfo(..)
    , State(..)
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

data RenderingInfo = RenderingInfo
    { position :: Position
    , color :: Color
    , size :: Size
    }

data Unit = Unit
    { unitPosition :: Position
    , unitColor :: Color
    , unitSize :: Size
    }

data Building = Building
    { buildingPosition :: Position
    , buildingColor :: Color
    , buildingSize :: Size
    }

data State = State
    { units :: Vector Unit
    , buildings :: Vector Building
    }

data Input = Input
    { keyboard :: Map Scancode ()
    }

type DTime = NominalDiffTime
