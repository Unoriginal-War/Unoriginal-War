{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where


import Control.Concurrent
import Control.Lens hiding (element)
import Control.Monad
import Data.Int
import Data.IORef
import Data.Monoid
import Data.Word
import qualified Data.Array.IArray as Array
import qualified Data.HashMap.Strict as HashMap
import Foreign.C
import Linear
import Linear.Affine
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import SDL.Cairo
import SDL.Cairo.Canvas
import SDL.Event
import SDL.Init (initializeAll)
import SDL.Input
import SDL.Video (createWindow, defaultWindow, createRenderer)
import SDL.Video.Renderer

import Game
import Grid
import Item
import Render
import Resource
import State
import Time
import Types hiding (Event)
import qualified Types as T


main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    -- TODO proper loading
    let unitDesc = _unitDescription $ Vector.head units'
    let buildingDesc = _buildingDescription $ Vector.head buildings'
    unitTex <- loadUnitTexture renderer unitDesc
    buildingTex <- loadBuildingTexture renderer buildingDesc

    state <- newMVar State
        { _units = units'
        , _buildings = buildings'
        , _grid = Grid
            { _verticalExtent = 10
            , _horizontalExtent = 10
            , _tiles = Array.array (0, 0) []
            }
        , _store = Store
            { _units = HashMap.insert unitDesc unitTex HashMap.empty
            , _buildings = HashMap.insert buildingDesc buildingTex HashMap.empty
            , _tiles = HashMap.empty
            }
        }
    input <- newIORef emptyInput

    run renderDelta $ render renderer state
    run gameDelta $ game input state
    void . loop inputDelta $ pollInput input
  where
    run delta = void . forkIO . loop delta
    renderDelta = 0.01 -- 100 FPS
    gameDelta = 0.001  -- 1000 FPS
    inputDelta = 0.001 -- 1000 FPS

    makeTexture :: Renderer -> Size -> FilePath -> IO Texture
    makeTexture renderer size' name = do
        texture <- createCairoTexture renderer (V2 iSize iSize)
        withCanvas texture $ do
            buildingImage <- loadImagePNG name
            image' buildingImage (D 0 0 dSize dSize)
        pure texture
      where
        iSize = CInt $ fromIntegral size'
        dSize = fromIntegral size'

    loadUnitTexture :: Renderer -> UnitDescription -> IO Texture
    loadUnitTexture renderer UnitDescription{..} =
        makeTexture renderer _unitSize _unitTexture

    loadBuildingTexture :: Renderer -> BuildingDescription -> IO Texture
    loadBuildingTexture renderer BuildingDescription{..} =
        makeTexture renderer _buildingSize _buildingTexture

    buildings' = Vector.fromList
        [ Building
            { _buildingDescription = BuildingDescription
                { _buildingSize = 200
                , _buildingTexture = "image/building.png"
                }
            , _buildingPosition = V2 200 200
            }
        ]

    units' = Vector.fromList
        [ Unit
            { _unitDescription = UnitDescription
                { _unitSpeed = 0.5
                , _unitSize = 50
                , _unitTexture = "image/unit.png"
                }
                , _unitPosition = V2 500 200
                , _unitPlan = cycle
                    [ MoveTo $ V2 200 400
                    , MoveTo $ V2 500 200
                    , MoveTo $ V2 500 500
                    ]
            }
        ]

updateInput :: KeyboardModifiers -> Event -> Input -> Input
updateInput km event input =
    case eventPayload event of
        KeyboardEvent keyboardEvent ->
            case keyboardEventKeyMotion keyboardEvent of
                Pressed -> modifyKyeboard $ Map.insert scanCode ()
                Released -> modifyKyeboard $ Map.delete scanCode
              where scanCode = keysymScancode $ keyboardEventKeysym keyboardEvent
        MouseMotionEvent MouseMotionEventData{..} ->
            setMousePos (mousePosToV2 mouseMotionEventPos) . modifyEvents .
                maybe ([] ++) (constrMouseMove mouseMotionEventPos)
                    . sequence $ fmap buttonToButton mouseMotionEventState
        MouseButtonEvent MouseButtonEventData{..} ->
            modifyEvents . maybe ([] ++)
                (\b ->case mouseButtonEventMotion of
                      Pressed -> constrMousePress mouseButtonEventPos b
                      Released -> generateClicks mouseButtonEventPos b mouseButtonEventClicks . constrMouseRelease mouseButtonEventPos b
                ) $ buttonToButton mouseButtonEventButton
        _ -> input
  where
    constrMouseMove p b = ([MouseMove (mousePosToV2 p) b km] <>)
    constrMousePress p b = ([ButtonsPressed (mousePosToV2 p) b km] <>)
    constrMouseRelease p b = ([ButtonsReleased (mousePosToV2 p) b km] <>)

    generateClicks
        :: Point V2 Int32
        -> Button
        -> Word8
        -> [T.Event]
        -> [T.Event]
    generateClicks p b = \case
        1 -> ([MouseClicked (mousePosToV2 p) b km] <>)
        2 -> ([MouseDoubleClicked (mousePosToV2 p) b km] <>)
        _ -> ([] <>)

    buttonToButton :: MouseButton -> Maybe Button
    buttonToButton = \case
        ButtonLeft -> Just LeftButton
        ButtonMiddle -> Just MiddleButton
        ButtonRight -> Just RightButton
        _ -> Nothing

    mousePosToV2 :: Point V2 Int32 -> V2 Int
    mousePosToV2 (P vect) = fmap fromIntegral vect

    modifyKyeboard f = over (Types.properties . keyboard) f input
    modifyEvents f = over events f input

    setMousePos :: V2 Int -> Input -> Input
    setMousePos = set (T.properties . mousePos)


pollInput :: IORef Input -> IO ()
pollInput input = do
    es <- pollEvents
    km <- fmap sdlKeyModifierToKeyboardModifier getModState
    let update = makeIORefTouple . foldl (comp km) id es
    -- Rely only on the mvarModification may be dangerous !!!
    -- Nobody is promising as that the value will be presented in the MVar in
    -- the first place, but most importunely the modifyMVar may not be atomic!!!
    -- See: https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Concurrent-MVar.html#v:modifyMVar
    atomicModifyIORef' input (update)
  where
    comp km up ev = updateInput km ev . up
    makeIORefTouple v = (v,())

    sdlKeyModifierToKeyboardModifier :: KeyModifier -> KeyboardModifiers
    sdlKeyModifierToKeyboardModifier KeyModifier{..} = KeyboardModifiers
        { _leftShift = keyModifierLeftShift
        , _rightShift = keyModifierRightShift
        , _leftCtrl = keyModifierLeftCtrl
        , _rightCtrl = keyModifierRightCtrl
        , _leftAlt = keyModifierLeftAlt
        , _rightAlt = keyModifierRightAlt
        , _leftGUI = keyModifierLeftGUI
        , _rightGUI = keyModifierRightGUI
        , _numLock = keyModifierNumLock
        , _capsLock = keyModifierCapsLock
        , _altGr = keyModifierAltGr
        }

{-

Architecture:

1. Game state.
The basic idea is that there's a function updating the game state.
This function will run in its own thread at a certain speed (meaning
the sample rate of the state).
Regarding the updates, they will happen by means of passage of time
and user input. Let's call it a controlled simulation.

2. Rendering
Another function will take care of rendering the state (either directly
or to something like diagrams, which will be subsequently passed to
the actual backend). This function will run in another thread and
another rate and will simply pick whatever state is currently available.

3. Input
a) listen to events
b) poll state


Note: I'm not sure how FRP is supposed to help us here.
However, I can see how a SAC could save a lot of recomputation in
rendering (a la React) and perhaps in game stepping too.

-}

