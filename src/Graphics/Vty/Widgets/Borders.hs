-- |This module provides visual borders to be placed between and
-- around widgets.
module Graphics.Vty.Widgets.Borders
    ( vBorder
    , hBorder
    , vBorderWith
    , hBorderWith
    , bordered
    )
where

import Graphics.Vty
    ( Attr
    , DisplayRegion(DisplayRegion)
    , Image
    , char_fill
    , region_height
    , region_width
    , image_width
    , image_height
    , vert_cat
    , horiz_cat
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    )
import Graphics.Vty.Widgets.Base
    ( (<++>)
    )
import Graphics.Vty.Widgets.Text
    ( simpleText
    )

-- |Create a single-row horizontal border.
hBorder :: Attr -> Widget
hBorder = hBorderWith '-'

-- |Create a single-row horizontal border using the specified
-- attribute and character.
hBorderWith :: Char -> Attr -> Widget
hBorderWith ch att =
    Widget { growVertical = False
           , growHorizontal = True
           , primaryAttribute = att
           , withAttribute = hBorder
           , render = \s -> char_fill att ch (region_width s) 1
           }

-- |Create a single-column vertical border.
vBorder :: Attr -> Widget
vBorder = vBorderWith '|'

-- |Create a single-column vertical border using the specified
-- attribute and character.
vBorderWith :: Char -> Attr -> Widget
vBorderWith ch att =
    Widget { growHorizontal = False
           , growVertical = True
           , primaryAttribute = att
           , render = \s -> char_fill att ch 1 (region_height s)
           , withAttribute = vBorder
           }

-- |Wrap a widget in a bordering box using the specified attribute.
bordered :: Attr -> Widget -> Widget
bordered att w = Widget {
                   growVertical = growVertical w
                 , growHorizontal = growHorizontal w
                 , primaryAttribute = att
                 , withAttribute = \att' -> bordered att' (withAttribute w att')
                 , render = renderBordered att w
                 }

renderBordered :: Attr -> Widget -> DisplayRegion -> Image
renderBordered att w s =
    -- Render the contained widget with enough room to draw borders.
    -- Then, use the size of the rendered widget to constrain the
    -- space used by the (expanding) borders.
    vert_cat [topBottom, middle, topBottom]
        where
          constrained = DisplayRegion (region_width s - 2) (region_height s - 2)
          renderedChild = render w constrained
          adjusted = DisplayRegion
                     (image_width renderedChild + 2)
                     (image_height renderedChild)
          corner = simpleText att "+"
          topBottom = render (corner <++> hBorder att <++> corner) adjusted
          leftRight = render (vBorder att) adjusted
          middle = horiz_cat [leftRight, renderedChild, leftRight]
