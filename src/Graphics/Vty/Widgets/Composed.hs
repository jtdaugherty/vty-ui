-- |This module provides high-level "combined" widgets which compose
-- the basic widget types to provide more interesting widgets.
module Graphics.Vty.Widgets.Composed
    ( bottomPadded
    , topPadded
    )
where

import Graphics.Vty.Widgets.Rendering ( Widget(..) )
import Graphics.Vty.Widgets.Base
    ( (<-->)
    , vFill
    , Box
    )

-- |Add expanding bottom padding to a widget.
bottomPadded :: (Widget a) => a -> Box
bottomPadded w = w <--> vFill (primaryAttribute w) ' '

-- |Add expanding top padding to a widget.
topPadded :: (Widget a) => a -> Box
topPadded w = vFill (primaryAttribute w) ' ' <--> w