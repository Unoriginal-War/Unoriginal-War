{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module State
    ( Building
    , State(..)
    , Unit
    , buildings
    , grid
    , store
    , units
    , mapSize
    , viewPort
    )
where

import Control.Lens
import Linear.V2
import Data.Vector

import Item
import Grid
import Resource hiding (units, buildings)
import ViewPort


data State = State
    { _units :: Vector Unit
    , _buildings :: Vector Building
    , _grid :: Grid
    , _store :: Store
    , _mapSize :: V2 Int
    , _viewPort :: ViewPort
    }

makeLenses ''State
