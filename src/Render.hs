{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Render
    ( render )
where

import Control.Concurrent
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Vector as Vector
import Foreign.C.Types
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import SDL (($=))
import SDL.Video.Renderer

import Item
import qualified Resource
import State
import ViewPort


makeRect :: (Num a) => Point V2 a -> a -> Rectangle a
makeRect (P (V2 x y)) h = Rectangle (P $ V2 (x - h) (y - h)) (V2 h h)

-- FIXME do something better than fromJust...
unitToInfo :: Resource.Store -> Unit -> RenderingInfo
unitToInfo store' Unit{..} = RenderingInfo
    { _rPosition = _unitPosition
    , _rTexture = fromJust $ HashMap.lookup _unitDescription (Resource._units store')
    , _rSize = _unitSize _unitDescription
    }

buildingToInfo :: Resource.Store -> Building -> RenderingInfo
buildingToInfo store' Building{..} = RenderingInfo
    { _rPosition = _buildingPosition
    , _rTexture = fromJust $ HashMap.lookup _buildingDescription (Resource._buildings store')
    , _rSize = _buildingSize _buildingDescription
    }

renderItem :: Renderer -> ViewPort -> RenderingInfo -> IO ()
renderItem renderer ViewPort{..} RenderingInfo{..} = do
    copy renderer _rTexture Nothing (Just $ makeRect (P iPos) iSize)
  where
    myRound :: RealFrac a => a -> Integer
    myRound = round

    transformToViewPort i = (fmap toInt i) - _position
    toInt = fromIntegral . myRound
    iPos = fmap fromIntegral $ transformToViewPort _rPosition
    iSize = CInt $ fromIntegral _rSize

render :: Renderer -> MVar State -> IO ()
render renderer state = do
    State{..} <- readMVar state

    let unitsInfo = Vector.map (unitToInfo $ _store) _units
    let buildingsInfo = Vector.map (buildingToInfo $ _store) _buildings

    -- Debug unit positions
    -- forM_ (units s) $ \unit -> print $ unitPosition unit

    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    sequence_ $ Vector.map (renderItem renderer _viewPort) (unitsInfo <> buildingsInfo)

    present renderer

