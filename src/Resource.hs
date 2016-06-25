{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Resource
    ( BuildingResources
    , Store(..)
    , TileResources
    , UnitResources
    , units
    , buildings
    , tiles
    )
where

import Control.Lens
import Data.HashMap.Strict
import SDL

import Item
import Grid hiding (tiles)

{-

Resource indexing:
* Units -> Soldier
* Buildings -> House
* Tiles -> Rocks

Resource using:
1. Describe unit/building/tile.
2. Describe the map (which units/building/tiles are used).
3. Load all images into the resource store and index them.
4. During rendering look up the key for the given unit/building/tile.

-}


type BuildingResources = HashMap BuildingDescription Texture
type TileResources = HashMap TileDescription Texture
type UnitResources = HashMap UnitDescription Texture

data Store = Store
    { _units :: UnitResources
    , _buildings :: BuildingResources
    , _tiles :: TileResources
    }

makeLenses ''Store
