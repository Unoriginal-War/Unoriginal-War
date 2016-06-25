{-# LANGUAGE TemplateHaskell #-}

module Grid
    ( Grid(..)
    , Tile(..)
    , TileDescription(..)
    , Feature(..)
    , verticalExtent
    , horizontalExtent
    , tiles
    , kind
    , speed
    , features
    , textureFile
    , size
    , tracks
    )
where

import Control.Lens hiding (Index)
import Data.Array.IArray
import Data.Word


type Count = Word16
type Index = Word16
type Size = Word16
type Speed = Float
type Map = Array Index Tile

data Grid = Grid
    { _verticalExtent :: Count
    , _horizontalExtent :: Count
    , _tiles :: Map
    }

data Tile = Tile
    { _kind :: TileDescription
    , _speed :: Speed
    , _features :: [Feature]
    }

data TileDescription = TileDescription
     { _textureFile :: FilePath
     , _size :: Size
     , _tracks :: () -- TODO used to display unit tracks over the tile
     }

-- TODO add more kinds and their properties
data Feature = Grass

makeLenses ''Grid
makeLenses ''Tile
makeLenses ''TileDescription
