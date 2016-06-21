{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Game
    ( game )
where

import Control.Concurrent
import qualified Data.Vector as Vector
import Linear

import Types

updateUnit :: Unit -> Unit
updateUnit u@Unit{..} = u
    { unitPosition = newPosition
    , unitPlan = newPlan
    }
  where
    (newPosition, newPlan) = carryOut unitPlan unitStatic unitPosition

carryOut :: [Action] -> StaticInfo -> Position -> (Position, [Action])
carryOut (x:xs) info pos = case x of
    MoveTo dest -> if distance dest pos < 2
                      then (pos, xs)
                   else (pos `approach` dest, x:xs)
  where
    approach :: Position -> Position -> Position
    approach from to = from + staticSpeed info *^ normalize (to - from)
carryOut [] _ pos = (pos, [])

update :: Input -> State -> State
update _input State{..} = state'
  where
    state' = State units' buildings

    units' = Vector.map updateUnit units

game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
