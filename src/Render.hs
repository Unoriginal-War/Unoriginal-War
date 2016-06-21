{-# LANGUAGE RecordWildCards #-}
module Render
    ( render )
where

import Control.Concurrent
import Data.Monoid ((<>))
import qualified Data.Vector as Vector
import Foreign.C.Types
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import SDL (($=))
import SDL.Cairo
import SDL.Cairo.Canvas
import SDL.Video.Renderer

import Types


makeRect :: (Num a) => Point V2 a -> a -> Rectangle a
makeRect (P (V2 x y)) h = Rectangle (P $ V2 (x - h) (y - h)) (V2 h h)

unitToInfo :: Unit -> RenderingInfo
unitToInfo Unit{..} = RenderingInfo
    { position = unitPosition
    , color = staticColor unitStatic
    , size = staticSize unitStatic
    }

buildingToInfo :: Building -> RenderingInfo
buildingToInfo Building{..} = RenderingInfo
    { position = buildingPosition
    , color = staticColor buildingStatic
    , size = staticSize buildingStatic
    }

renderEntity :: Renderer -> RenderingInfo -> IO ()
renderEntity renderer RenderingInfo{..} = do
    let toCInt = CInt . fromIntegral . myRound
    let iPos = fmap toCInt position

    texture <- createCairoTexture renderer (V2 iSize iSize)
    withCanvas texture $ do
        buildingImage <- loadImagePNG "./image/building.png"
        image' buildingImage (D 0 0 dSize dSize)

    copy renderer texture Nothing (Just $ makeRect (P iPos) iSize)
  where
    myRound :: RealFrac a => a -> Integer
    myRound = round

    iSize = CInt $ fromIntegral size
    dSize :: Double
    dSize = fromIntegral size

render :: Renderer -> MVar State -> IO ()
render renderer state = do
    s <- readMVar state

    let unitsInfo = Vector.map unitToInfo (units s)
    let buildingsInfo = Vector.map buildingToInfo (buildings s)

    -- Debug unit positions
    -- forM_ (units s) $ \unit -> print $ unitPosition unit

    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    sequence_ $ Vector.map (renderEntity renderer) (unitsInfo <> buildingsInfo)

    present renderer

