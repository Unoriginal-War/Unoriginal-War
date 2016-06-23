{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Resource
    ( BuildingResources
    , Store(..)
    , TileResources
    , UnitResources
    )
where

import Data.HashMap.Strict
import SDL

import Item
import Grid

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
    { units :: UnitResources
    , buildings :: BuildingResources
    , tiles :: TileResources
    }
