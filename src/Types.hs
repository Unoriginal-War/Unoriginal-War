{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Types
    ( Button(..)
    , DTime
    , Event(..)
    , Input(..)
    , KeyboardModifiers(..)
    , Properties(..)
    , clickButton
    , clickKeyModifiers
    , clickPos
    , doubleClickButton
    , doubleClickKeyModifiers
    , doubleClickPos
    , emptyInput
    , events
    , foldInputEvents
    , keyboard
    , mousePos
    , moveButton
    , moveKeyModifiers
    , movePos
    , pressButton
    , pressedButtons
    , pressKeyModifiers
    , pressPos
    , properties
    , releaseButton
    , releaseKeyModifiers
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
  deriving (Show)

data KeyboardModifiers = KeyboardModifiers
    { _leftShift :: Bool
    , _rightShift :: Bool
    , _leftCtrl :: Bool
    , _rightCtrl :: Bool
    , _leftAlt :: Bool
    , _rightAlt :: Bool
    , _leftGUI :: Bool
    , _rightGUI :: Bool
    , _numLock :: Bool
    , _capsLock :: Bool
    , _altGr :: Bool
    }
  deriving (Show)

-- We should change the name according the event use to enable better mapping ;)
data Event =
  MouseMove
    { _movePos :: !MousePos
    , _moveButton :: ![Button]
    , _moveKeyModifiers :: KeyboardModifiers
    }
  | ButtonsPressed
    { _pressPos :: !MousePos
    , _pressButton :: !Button
    , _pressKeyModifiers :: KeyboardModifiers
    }
  | ButtonsReleased
    { _releasePos :: !MousePos
    , _releaseButton :: !Button
    , _releaseKeyModifiers :: KeyboardModifiers
    }
  | MouseClicked
    { _clickPos :: !MousePos
    , _clickButton :: !Button
    , _clickKeyModifiers :: KeyboardModifiers
    }
  | MouseDoubleClicked
    { _doubleClickPos :: !MousePos
    , _doubleClickButton :: !Button
    , _doubleClickKeyModifiers :: KeyboardModifiers
    }
  deriving (Show)

data Input = Input
    { _properties :: !Properties
    , _events :: ![Event]
    }
  deriving (Show)

data Properties = Properties
    { _mousePos :: !MousePos
    -- | This map should be probably replace with some structure better
    -- describing the structure and purpose of the keyboard inputs (like
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

foldInputEvents
    :: (Properties -> Event -> a -> a)
    -> Input
    -> a
    -> a
foldInputEvents f (Input p es) s =
  foldr (f p) s es

makeLenses ''Event
makeLenses ''Input
makeLenses ''Properties
