{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Game
    ( game )
where

import Control.Concurrent
import Control.Lens
import Data.IORef
import Linear
import qualified Data.Vector as Vector

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
    approach from' to' = from' + _unitSpeed features *^ normalize (to' - from')

carryOut [] _ pos = (pos, [])

runGameStep :: State -> State
runGameStep s@State{..} = s{ _units = Vector.map updateUnit _units }

processInputs :: Properties -> Event -> State -> State
processInputs _ e s =
    case e of
        MouseClicked{..} -> case _clickButton of
            RightButton -> set (units . ix 0 . unitPlan)
                ([MoveTo (fmap fromIntegral _clickPos)]) s
            _ -> s
        MouseDoubleClicked{..} -> s
        _ -> s

getInput :: Input -> (Input, Input)
getInput i = (i { _events = mempty }, i)

game :: IORef Input -> MVar State -> IO ()
game input state = do
    i <- atomicModifyIORef' input getInput
    print i
    -- First we modify the state based on the input received till this game
    -- turn.

    -- Rely only on the mvarModification may be dangerous !!!
    -- Nobody is promising as that the value will be presented in the MVar in
    -- the first place, but most importunely the modifyMVar may not be atomic!!!
    -- See: https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Concurrent-MVar.html#v:modifyMVar
    --modifyMVar_ state (pure . processInputs i)
    -- Second we do the actual game step

    -- Rely only on the mvarModification may be dangerous !!!
    -- Nobody is promising as that the value will be presented in the MVar in
    -- the first place, but most importunely the modifyMVar may not be atomic!!!
    -- See: https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Concurrent-MVar.html#v:modifyMVar
    modifyMVar_ state (pure . runGameStep . foldInputEvents processInputs i)
