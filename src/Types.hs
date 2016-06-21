{-# LANGUAGE TemplateHaskell #-}
module Types
    ( Button(..)
    , DTime
    , Input(..)
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
import Linear
import SDL.Input (Scancode)


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
