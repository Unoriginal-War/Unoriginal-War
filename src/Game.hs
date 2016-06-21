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
import Types

updateUnitProps :: a ~ 'UnitType => Description a -> Properties a -> Properties a
updateUnitProps desc u@UnitProperties{..} = u
    { position = newPosition
    , plan = newPlan
    }
  where
    (newPosition, newPlan) = carryOut plan (features desc) position

carryOut :: [Action] -> (Features 'UnitType) -> Position -> (Position, [Action])
carryOut (x:xs) features pos = case x of
    MoveTo dest -> if distance dest pos < 2
                      then (pos, xs)
                   else (pos `approach` dest, x:xs)
  where
    approach :: Position -> Position -> Position
    approach from to = from + speed features *^ normalize (to - from)
carryOut [] _ pos = (pos, [])

update :: Input -> State -> State
update _input State{..} = state'
  where
    state' = State units' buildings

    units' = Vector.map updateUnit units

    updateUnit u = u
        { Item.properties = updateUnitProps (description u) (Item.properties u)
        }

game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
