{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where


import Control.Concurrent
import Control.Lens hiding (element)
import Control.Monad
import Data.Int
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
    let unitDesc = unitDescription $ Vector.head units'
    let buildingDesc = buildingDescription $ Vector.head buildings'
    unitTex <- loadUnitTexture renderer unitDesc
    buildingTex <- loadBuildingTexture renderer buildingDesc

    state <- newMVar State
        { units = units'
        , buildings = buildings'
        , grid = Grid
            { verticalExtent = 10
            , horizontalExtent = 10
            , tiles = Array.array (0, 0) []
            }
        , store = Store
            { units = HashMap.insert unitDesc unitTex HashMap.empty
            , buildings = HashMap.insert buildingDesc buildingTex HashMap.empty
            , tiles = HashMap.empty
            }
        }
    input <- newMVar emptyInput

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
        makeTexture renderer unitSize unitTexture

    loadBuildingTexture :: Renderer -> BuildingDescription -> IO Texture
    loadBuildingTexture renderer BuildingDescription{..} =
        makeTexture renderer buildingSize buildingTexture

    buildings' = Vector.fromList
        [ Building
            { buildingDescription = BuildingDescription
                { buildingSize = 200
                , buildingTexture = "image/building.png"
                }
            , buildingPosition = V2 200 200
            }
        ]

    units' = Vector.fromList
        [ Unit
            { unitDescription = UnitDescription
                { unitSpeed = 0.5
                , unitSize = 50
                , unitTexture = "image/unit.png"
                }
                , unitPosition = V2 500 200
                , unitPlan = cycle
                    [ MoveTo $ V2 200 400
                    , MoveTo $ V2 500 200
                    , MoveTo $ V2 500 500
                    ]
            }
        ]

updateInput :: Event -> Input -> Input
updateInput event input =
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
                      Released -> constrMousePress mouseButtonEventPos b
                      Pressed -> constrMouseRelease mouseButtonEventPos b
                ) $ buttonToButton mouseButtonEventButton
        _ -> input
  where
    constrMouseMove p b = ([MouseMove (mousePosToV2 p) b] ++)
    constrMousePress p b = ([ButtonsPressed (mousePosToV2 p) b] ++)
    constrMouseRelease p b = ([ButtonsReleased (mousePosToV2 p) b] ++)

    buttonToButton :: MouseButton -> Maybe Button
    buttonToButton = \case
        ButtonLeft -> Just LeftButton
        ButtonMiddle -> Just MiddleButton
        ButtonRight -> Just RightButton
        _ -> Nothing

    mousePosToV2 :: Point V2 Int32 -> V2 Int
    mousePosToV2 (P vect) = fmap fromIntegral vect

    modifyKyeboard f = over (Types.properties . keyboard) f input
    modifyEvents f = over (events) f input

    setMousePos :: V2 Int -> Input -> Input
    setMousePos = set (T.properties . mousePos)

pollInput :: MVar Input -> IO ()
pollInput input = do
    es <- pollEvents
    let update = foldl comp id es
    modifyMVar_ input (return . update)
  where
    comp up ev = updateInput ev . up
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

