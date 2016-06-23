{-# LANGUAGE DataKinds #-}
module State
    ( Building
    , State(..)
    , Unit
    )
where

import Data.Vector

import Item
import Grid
import Resource


data State = State
    { units :: Vector Unit
    , buildings :: Vector Building
    , grid :: Grid
    , store :: Store
    }
