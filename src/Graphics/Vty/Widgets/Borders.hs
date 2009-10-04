{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides visual borders to be placed between and
-- around widgets.
module Graphics.Vty.Widgets.Borders
    ( VBorder
    , HBorder
    , Bordered
    , vBorder
    , hBorder
    , bordered
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
    , (<++>)
    , (<-->)
    , text
    )

-- |A vertical border to be placed between horizontally-oriented
-- widgets.  See 'vBorder'.
data VBorder = VBorder Attr

-- |A horizontal border to be placed between vertically-oriented
-- widgets.  See 'hBorder'.
data HBorder = HBorder Attr

-- |A container widget which draws a border around all four sides of
-- the widget it contains.  See 'bordered'.
data Bordered = forall a. (Widget a) => Bordered Attr a

instance Widget VBorder where
    growVertical _ = True
    growHorizontal _ = False
    primaryAttribute (VBorder a) = a
    render s (VBorder att) =
        char_fill att '|' 1 (region_height s)
    withAttribute _ att = VBorder att

instance Widget HBorder where
    growVertical _ = False
    growHorizontal _ = True
    primaryAttribute (HBorder a) = a
    render s (HBorder att) =
        char_fill att '-' (region_width s) 1
    withAttribute _ att = HBorder att

instance Widget Bordered where
    growVertical (Bordered _ w) = growVertical w
    growHorizontal (Bordered _ w) = growHorizontal w
    primaryAttribute (Bordered _ w) = primaryAttribute w
    -- This is consistent with primaryAttribute here, but it probably
    -- should be the border attribute, not the child primaryAttribute.
    withAttribute (Bordered a w) att = Bordered a (withAttribute w att)
    render s (Bordered att w) =
        render s (topBottom <--> middle <--> topBottom)
            where
              topBottom = corner <++> hBorder att <++> corner
              middle = vBorder att <++> w <++> vBorder att
              corner = text att "+"

-- |Create a horizontal border.
hBorder :: Attr -> HBorder
hBorder = HBorder

-- |Create a vertical border.
vBorder :: Attr -> VBorder
vBorder = VBorder

-- |Wrap a widget in a border using the specified attribute.
bordered :: (Widget a) => Attr -> a -> Bordered
bordered = Bordered