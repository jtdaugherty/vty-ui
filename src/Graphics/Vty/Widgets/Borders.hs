{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides visual borders to be placed between and
-- around widgets.
module Graphics.Vty.Widgets.Borders
    ( Border
    , Bordered
    , vBorder
    , hBorder
    , bordered
    )
where

import Graphics.Vty
    ( Attr
    , DisplayRegion(DisplayRegion)
    , (<|>)
    , char_fill
    , region_height
    , region_width
    , image_width
    , image_height
    , vert_cat
    )
import Graphics.Vty.Widgets.Base
    ( Widget(..)
    , (<++>)
    , text
    )

-- |A horizontal or vertical border to be placed between widgets.  See
-- 'hBorder' and 'vBorder'.
data Border = VBorder Attr
            | HBorder Attr

-- |A container widget which draws a border around all four sides of
-- the widget it contains.  See 'bordered'.
data Bordered = forall a. (Widget a) => Bordered Attr a

instance Widget Border where
    growVertical (VBorder _) = True
    growVertical (HBorder _) = False

    growHorizontal (VBorder _) = False
    growHorizontal (HBorder _) = True

    primaryAttribute (VBorder a) = a
    primaryAttribute (HBorder a) = a

    render s (VBorder att) =
        char_fill att '|' 1 (region_height s)
    render s (HBorder att) =
        char_fill att '-' (region_width s) 1

    withAttribute (VBorder _) att = VBorder att
    withAttribute (HBorder _) att = HBorder att

instance Widget Bordered where
    growVertical (Bordered _ w) = growVertical w
    growHorizontal (Bordered _ w) = growHorizontal w
    primaryAttribute (Bordered att _) = att
    withAttribute (Bordered _ w) att = Bordered att (withAttribute w att)
    render s (Bordered att w) =
        -- Render the contained widget with enough room to draw
        -- borders.  Then, use the size of the rendered widget to
        -- constrain the space used by the (expanding) borders.
        vert_cat [topBottom, middle, topBottom]
            where
              constrained = DisplayRegion (region_width s - 2) (region_height s - 2)
              renderedChild = render constrained w
              adjusted = DisplayRegion
                         (image_width renderedChild + 2)
                         (image_height renderedChild)
              corner = text att "+"
              topBottom = render adjusted (corner <++> hBorder att <++> corner)
              leftRight = render adjusted $ vBorder att
              middle = leftRight <|> renderedChild <|> leftRight

-- |Create a single-row horizontal border.
hBorder :: Attr -> Border
hBorder = HBorder

-- |Create a single-column vertical border.
vBorder :: Attr -> Border
vBorder = VBorder

-- |Wrap a widget in a bordering box using the specified attribute.
bordered :: (Widget a) => Attr -> a -> Bordered
bordered = Bordered