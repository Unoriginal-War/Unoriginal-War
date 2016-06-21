{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where


import Control.Concurrent
import Control.Lens hiding (element)
import Control.Monad
import Linear
import Linear.Affine
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import SDL.Event
import SDL.Init (initializeAll)
import SDL.Input
import SDL.Video (createWindow, defaultWindow, createRenderer)
import SDL.Video.Renderer

import Time
import Types hiding (Event)
import qualified Types as T
import Render
import Game


main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    state <- newMVar $ State units' buildings'
    input <- newMVar $ emptyInput

    run renderDelta $ render renderer state
    run gameDelta $ game input state
    void . loop inputDelta $ pollInput input
  where
    run delta = void . forkIO . loop delta
    renderDelta = 0.01 -- 100 FPS
    gameDelta = 0.001  -- 1000 FPS
    inputDelta = 0.001 -- 1000 FPS

    buildings' = Vector.fromList
        [ Building 300 (StaticInfo 128 200 0)
        , Building 500 (StaticInfo 200 100 0)
        ]

    units' = Vector.fromList
        [ Unit (V2 500 200) (StaticInfo 255 50 0.5)
          (cycle [MoveTo $ V2 200 400, MoveTo $ V2 500 200, MoveTo $ V2 500 500])
        , Unit (V2 600 200) (StaticInfo 255 20 2)
          (cycle [MoveTo $ V2 200 400, MoveTo $ V2 600 200])
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
            modifyEvents ([MouseMove
                (mousePosToV2 mouseMotionEventPos)
                (fmap buttonToButton mouseMotionEventState)] ++)
        _ -> input
  where
    buttonToButton :: MouseButton -> Button
    buttonToButton = \case
        ButtonLeft -> LeftButton
        ButtonMiddle -> RightButton
        ButtonRight -> MiddleButton
        _ -> T.Unknown

    mousePosToV2 (P vect) = fmap fromIntegral vect
    modifyKyeboard f = over (properties . keyboard) f input
    modifyEvents f = over (events) f input

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

