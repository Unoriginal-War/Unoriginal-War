{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Game
    ( game )
where

import Control.Concurrent
import Control.Monad
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear
import SDL.Input.Keyboard

import Time
import Types


update :: Input -> State -> State
update input state = state

game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
