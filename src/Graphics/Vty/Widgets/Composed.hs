-- |This module provides high-level composed widgets.
module Graphics.Vty.Widgets.Composed
    ( bottomPadded
    , topPadded
    , boxLimit
    )
where

import Graphics.Vty
    ( Attr
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget
    )
import Graphics.Vty.Widgets.Base
    ( Box
    , VFill
    , VLimit
    , HLimit
    , (<-->)
    , vFill
    , vLimit
    , hLimit
    )

-- |Add expanding bottom padding to a widget.
bottomPadded :: Widget a -> Attr -> Widget (Box a VFill)
bottomPadded w attr = w <--> vFill attr ' '

-- |Add expanding top padding to a widget.
topPadded :: Widget a -> Attr -> Widget (Box VFill a)
topPadded w attr = vFill attr ' ' <--> w

-- |Impose a maximum size (width, height) on a widget.
boxLimit :: Int -- ^Maximum width in columns
         -> Int -- ^Maximum height in rows
         -> Widget a
         -> Widget (VLimit (HLimit a))
boxLimit maxWidth maxHeight = vLimit maxHeight . hLimit maxWidth