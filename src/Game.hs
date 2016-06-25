{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Game
    ( game )
where

import Control.Concurrent
import qualified Data.Vector as Vector
import Linear

import Item
import State
import Types

updateUnit :: Unit -> Unit
updateUnit u@Unit{..} = u
    { _unitPosition = newPosition
    , _unitPlan = newPlan
    }
  where
    (newPosition, newPlan) = carryOut _unitPlan _unitDescription _unitPosition

carryOut :: [Action] -> UnitDescription -> Position -> (Position, [Action])
carryOut (x:xs) features pos = case x of
    MoveTo dest -> if distance dest pos < 2
                      then (pos, xs)
                   else (pos `approach` dest, x:xs)
  where
    approach :: Position -> Position -> Position
    approach from to = from + unitSpeed features *^ normalize (to - from)
carryOut [] _ pos = (pos, [])

update :: Input -> State -> State
update _input s@State{..} = s{ units = Vector.map updateUnit units }

game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
