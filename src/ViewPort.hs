{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module ViewPort
    ( ViewPort(..)
    , ViewPosition
    , size
    , position
    )
where

import Control.Lens
import Linear.V2

type ViewPosition = V2 Int

data ViewPort = ViewPort
-- TODO: Size should be probably loaded from the window.
    { _size :: V2 Int
    , _position :: ViewPosition
    }

makeLenses ''ViewPort
