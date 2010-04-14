-- |This module provides high-level composed widgets.
module Graphics.Vty.Widgets.Composed
    ( bottomPadded
    , topPadded
    , boxLimit
    )
where

import Graphics.Vty.Widgets.Rendering
    ( Widget(primaryAttribute)
    )
import Graphics.Vty.Widgets.Base
    ( (<-->)
    , vFill
    , vLimit
    , hLimit
    )

-- |Add expanding bottom padding to a widget.
bottomPadded :: Widget -> Widget
bottomPadded w = w <--> vFill (primaryAttribute w) ' '

-- |Add expanding top padding to a widget.
topPadded :: Widget -> Widget
topPadded w = vFill (primaryAttribute w) ' ' <--> w

-- |Impose a maximum size (width, height) on a widget.
boxLimit :: Int -- ^Maximum width in columns
         -> Int -- ^Maximum height in rows
         -> Widget
         -> Widget
boxLimit maxWidth maxHeight = vLimit maxHeight . hLimit maxWidth