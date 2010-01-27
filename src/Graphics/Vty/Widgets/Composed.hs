-- |This module provides high-level "combined" widgets which compose
-- the basic widget types to provide more interesting widgets.
module Graphics.Vty.Widgets.Composed
    ( bottomPadded
    , topPadded
    )
where

import Graphics.Vty.Widgets.Rendering
    ( Widget(primaryAttribute)
    )
import Graphics.Vty.Widgets.Base
    ( (<-->)
    , vFill
    )

-- |Add expanding bottom padding to a widget.
bottomPadded :: Widget -> Widget
bottomPadded w = w <--> vFill (primaryAttribute w) ' '

-- |Add expanding top padding to a widget.
topPadded :: Widget -> Widget
topPadded w = vFill (primaryAttribute w) ' ' <--> w