-- |This module provides high-level "combined" widgets which compose
-- the basic widget types to provide more interesting widgets.
module Graphics.Vty.Widgets.Composed
    ( bottomPadded
    , topPadded
    )
where

import Graphics.Vty.Widgets.Base
    ( Widget(..)
    , (<-->)
    , vFill
    , VBox
    )

-- |Add expanding bottom padding to a widget.
bottomPadded :: (Widget a) => a -> VBox
bottomPadded w = w <--> vFill (primaryAttribute w) ' '

-- |Add expanding top padding to a widget.
topPadded :: (Widget a) => a -> VBox
topPadded w = vFill (primaryAttribute w) ' ' <--> w