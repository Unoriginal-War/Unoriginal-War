{-# LANGUAGE TemplateHaskell #-}
module Types
    ( Action(..)
    , Building(..)
    , Button(..)
    , DTime
    , Input(..)
    , Position
    , RenderingInfo(..)
    , State(..)
    , StaticInfo(..)
    , Unit(..)
    , Event(..)
    , emptyInput
    , events
    , keyboard
    , mousePos
    , moveButton
    , movePos
    , pressButton
    , pressedButtons
    , pressPos
    , properties
    , releaseButton
    , releasePos
    )
  where

import Control.Lens hiding (element)
import qualified Data.Map.Strict as Map
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

type MousePos = V2 Int

-- We should change the name according the event use to enable better mapping ;)
data Button =
    LeftButton
    | RightButton
    | MiddleButton
    | Unknown
  deriving (Show)

-- We should change the name according the event use to enable better mapping ;)
data Event =
  MouseMove
    { _movePos :: MousePos
    , _moveButton :: [Button]
    }
  | ButtonsPressed
    { _pressPos :: MousePos
    , _pressButton :: [Button]
    }
  | ButtonsReleased
    { _releasePos :: MousePos
    , _releaseButton :: [Button]
    }
  deriving (Show)

data Input = Input
    { _properties :: Properties
    , _events :: [Event]
    }
  deriving (Show)

data Properties = Properties
    { _mousePos :: MousePos
    -- | This map should be probablu replace with some structure better
    -- describing the structure and purpose of the keboard impouts (like
    -- Attack,or Gurad).
    , _keyboard :: Map.Map Scancode ()
    , _pressedButtons :: Map.Map Button ()
    }
  deriving (Show)

type DTime = NominalDiffTime

emptyInput :: Input
emptyInput = Input
    { _properties = Properties
        { _mousePos = 0
        , _keyboard = Map.empty
        , _pressedButtons = Map.empty
        }
    , _events = []
    }

makeLenses ''Event
makeLenses ''Input
makeLenses ''Properties
