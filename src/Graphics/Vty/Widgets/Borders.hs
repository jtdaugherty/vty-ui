{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides visual borders to be placed between and
-- around widgets.
module Graphics.Vty.Widgets.Borders
    ( vBorder
    , hBorder
    , bordered
    )
where

import Graphics.Vty
    ( Attr
    , DisplayRegion(DisplayRegion)
    , char_fill
    , region_height
    , region_width
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Render
    , Orientation(..)
    , renderImg
    , renderMany
    , renderWidth
    , renderHeight
    )
import Graphics.Vty.Widgets.Base
    ( (<++>)
    )
import Graphics.Vty.Widgets.Text
    ( simpleText
    )

-- |Create a single-row horizontal border.
hBorder :: Attr -> Widget
hBorder att = Widget {
                growVertical = False
              , growHorizontal = True
              , primaryAttribute = att
              , withAttribute = hBorder
              , render = \s -> renderImg $ char_fill att '-' (region_width s) 1
              }

-- |Create a single-column vertical border.
vBorder :: Attr -> Widget
vBorder att = Widget {
                growHorizontal = False
              , growVertical = True
              , primaryAttribute = att
              , render = \s -> renderImg $ char_fill att '|' 1 (region_height s)
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

renderBordered :: Attr -> Widget -> DisplayRegion -> Render
renderBordered att w s =
    -- Render the contained widget with enough room to draw borders.
    -- Then, use the size of the rendered widget to constrain the
    -- space used by the (expanding) borders.
    renderMany Vertical [topBottom, middle, topBottom]
        where
          constrained = DisplayRegion (region_width s - 2) (region_height s - 2)
          renderedChild = render w constrained
          adjusted = DisplayRegion
                     (renderWidth renderedChild + 2)
                     (renderHeight renderedChild)
          corner = simpleText att "+"
          topBottom = render (corner <++> hBorder att <++> corner) adjusted
          leftRight = render (vBorder att) adjusted
          middle = renderMany Horizontal [leftRight, renderedChild, leftRight]
