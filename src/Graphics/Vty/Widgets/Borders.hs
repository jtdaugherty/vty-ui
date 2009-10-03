-- |This module provides visual borders to be placed between widgets.
module Graphics.Vty.Widgets.Borders
    ( VBorder
    , HBorder
    , vBorder
    , hBorder
    )
where

import Graphics.Vty
    ( Attr
    , char_fill
    , region_height
    , region_width
    )
import Graphics.Vty.Widgets.Base
    ( Widget(..)
    )

-- |A vertical border to be placed between horizontally-oriented
-- widgets.  See 'vBorder'.
data VBorder = VBorder Attr

-- |A horizontal border to be placed between vertically-oriented
-- widgets.  See 'hBorder'.
data HBorder = HBorder Attr

instance Widget VBorder where
    growVertical _ = True
    growHorizontal _ = False
    primaryAttribute (VBorder a) = a
    render s (VBorder att) =
        char_fill att '|' 1 (region_height s)

instance Widget HBorder where
    growVertical _ = False
    growHorizontal _ = True
    primaryAttribute (HBorder a) = a
    render s (HBorder att) =
        char_fill att '-' (region_width s) 1

-- |Create a horizontal border.
hBorder :: Attr -> HBorder
hBorder = HBorder

-- |Create a vertical border.
vBorder :: Attr -> VBorder
vBorder = VBorder