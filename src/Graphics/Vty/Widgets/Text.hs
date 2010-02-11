module Graphics.Vty.Widgets.Text
    ( text
    )
where

import Graphics.Vty
    ( Attr
    , DisplayRegion
    , region_height
    , region_width
    , string
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Render
    , renderImg
    )
import Graphics.Vty.Widgets.Base
    ( )

-- |A text widget consisting of a string rendered using an
-- attribute. See 'text'.
text :: Attr -> String -> Widget
text att content = Widget {
                     growHorizontal = False
                   , growVertical = False
                   , primaryAttribute = att
                   , withAttribute = flip text content
                   , render = renderText att content
                   }

renderText :: Attr -> String -> DisplayRegion -> Render
renderText att content sz = renderImg img
    where
      img = if region_height sz == 0
            then nullImg
            else string att truncated
      truncated = take (fromEnum $ region_width sz) content
      nullImg = string att ""
