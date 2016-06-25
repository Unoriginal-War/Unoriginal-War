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
    )
where

import Control.Lens
import Data.Vector

import Item
import Grid
import Resource hiding (units, buildings)


data State = State
    { _units :: Vector Unit
    , _buildings :: Vector Building
    , _grid :: Grid
    , _store :: Store
    }

makeLenses ''State
