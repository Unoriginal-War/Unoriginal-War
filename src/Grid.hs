module Grid
    ( Grid(..)
    , Tile(..)
    , TileDescription(..)
    , Feature(..)
    )
where

import Data.Array.IArray
import Data.Word


type Count = Word16
type Index = Word16
type Size = Word16
type Speed = Float
type Map = Array Index Tile

data Grid = Grid
    { verticalExtent :: Count
    , horizontalExtent :: Count
    , tiles :: Map
    }

data Tile = Tile
    { kind :: TileDescription
    , speed :: Speed
    , features :: [Feature]
    }

data TileDescription = TileDescription
     { textureFile :: FilePath
     , size :: Size
     , tracks :: () -- TODO used to display unit tracks over the tile
     }

-- TODO add more kinds and their properties
data Feature = Grass
