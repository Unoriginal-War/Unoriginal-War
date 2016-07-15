{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Game
    ( game
    , snapIfTrue
    )
where

import Control.Concurrent
import Control.Lens
import Data.IORef
import Data.Monoid
import Linear
import qualified Data.Vector as Vector

import Item
import State
import Types
import ViewPort

updateUnit :: Unit -> Unit
updateUnit u@Unit{..} = u
    { _unitPosition = newPosition
    , _unitPlan = newPlan
    }
  where
    (newPosition, newPlan) = carryOut _unitPlan _unitDescription _unitPosition

transformFromViewPort :: ViewPosition -> V2 Int -> V2 Int
transformFromViewPort pos xy = pos + xy

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
            RightButton ->
                case _leftShift _clickKeyModifiers of
                    True ->
                        over (units . ix 0 . unitPlan)
                        (\l -> l <> [createMoveTo _clickPos]) s
                    False ->
                        set (units . ix 0 . unitPlan)
                        ([createMoveTo _clickPos]) s
            _ -> s
        MouseDoubleClicked{..} -> s
        _ -> s
  where
    createMoveTo pos =
        MoveTo . fmap fromIntegral $ transformFromViewPort viewPortPos pos
    viewPortPos = s ^. (viewPort . position)

processProperties :: Properties -> State -> State
processProperties p@Properties{..} = moveViewPort p

moveViewPort :: Properties -> State -> State
moveViewPort Properties{..} = checkBottomViewMove
    . checkRigthViewMove
    . checkTopViewMove
    . checkLeftViewMove
  where
    supValue x = x - 1
    checkLeftViewMove :: State -> State
    checkLeftViewMove =
        if (_mousePos ^. _x) <= 20
            then (viewPort . position . _x) %~ (snapIfTrue 0 supValue (>))
            else id
    checkTopViewMove =
        if (_mousePos ^. _y) <= 20
            then (viewPort . position . _y) %~ (snapIfTrue 0 supValue (>))
            else id
    checkRigthViewMove s' =
        if (_mousePos ^. _x) >= screenSizeX - 20
            then over (viewPort . position . _x)
                (snapIfTrue (screenSizeX - mapSizeX) ((+)1) (>)) s'
            else id s'
      where
        screenSizeX = s' ^. (viewPort . size . _x)
        mapSizeX = s' ^. (mapSize . _x)
    checkBottomViewMove s' =
        if (_mousePos ^. _y) >= screenSizeY - 20
            then over (viewPort . position . _y)
                (snapIfTrue (screenSizeY - mapSizeY) ((+)1) (>)) s'
            else id s'
      where
        screenSizeY = s' ^. (viewPort . size . _y)
        mapSizeY = s' ^. (mapSize . _y)

snapIfTrue :: Ord a => a -> (a -> a) -> (a -> a -> Bool) -> a -> a
snapIfTrue boundary addFunc check v = let x = addFunc v in
        if check x boundary
            then x
            else boundary

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
    modifyMVar_ state (pure
        . runGameStep
        . processProperties (_properties i)
        . foldInputEvents processInputs i
        )
