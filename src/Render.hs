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


makeRect :: (Num a) => Point V2 a -> a -> Rectangle a
makeRect (P (V2 x y)) h = Rectangle (P $ V2 (x - h) (y - h)) (V2 h h)

-- FIXME do something better than fromJust...
unitToInfo :: Resource.Store -> Unit -> RenderingInfo
unitToInfo store Unit{..} = RenderingInfo
    { rPosition = unitPosition
    , rTexture = fromJust $ HashMap.lookup unitDescription (Resource.units store)
    , rSize = unitSize unitDescription
    }

buildingToInfo :: Resource.Store -> Building -> RenderingInfo
buildingToInfo store Building{..} = RenderingInfo
    { rPosition = buildingPosition
    , rTexture = fromJust $ HashMap.lookup buildingDescription (Resource.buildings store)
    , rSize = buildingSize buildingDescription
    }

renderItem :: Renderer -> RenderingInfo -> IO ()
renderItem renderer RenderingInfo{..} = do
    let toCInt = CInt . fromIntegral . myRound
    let iPos = fmap toCInt rPosition

    copy renderer rTexture Nothing (Just $ makeRect (P iPos) iSize)
  where
    myRound :: RealFrac a => a -> Integer
    myRound = round

    iSize = CInt $ fromIntegral rSize

render :: Renderer -> MVar State -> IO ()
render renderer state = do
    s <- readMVar state

    let unitsInfo = Vector.map (unitToInfo $ store s) (units s)
    let buildingsInfo = Vector.map (buildingToInfo $ store s) (buildings s)

    -- Debug unit positions
    -- forM_ (units s) $ \unit -> print $ unitPosition unit

    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    sequence_ $ Vector.map (renderItem renderer) (unitsInfo <> buildingsInfo)

    present renderer

