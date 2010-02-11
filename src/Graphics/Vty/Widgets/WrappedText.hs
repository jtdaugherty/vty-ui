-- |This module provides a widget which automatically wraps text in
-- the available space.  To create a 'WrappedText', see 'wrappedText'.
module Graphics.Vty.Widgets.WrappedText
    ( wrappedText
    )
where

import Text.Trans.Wrap ( wrap )
import Graphics.Vty
    ( Attr
    , DisplayRegion
    , region_width
    , region_height
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Orientation(..)
    , Render
    , renderMany
    )
import Graphics.Vty.Widgets.Base
    ( hFill
    )
import Graphics.Vty.Widgets.Text
    ( text
    )

-- |Create a wrapped text widget from the specified attribute and
-- text.
wrappedText :: Attr -> String -> Widget
wrappedText attr str = Widget {
                         growHorizontal = True
                       , growVertical = False
                       , primaryAttribute = attr
                       , withAttribute = flip wrappedText str
                       , render = renderWrappedText attr str
                       }

renderWrappedText :: Attr -> String -> DisplayRegion -> Render
renderWrappedText attr str s =
    let ws = map (\w -> render (convert w) s) $ lines wrapped
        wrapped = wrap (fromEnum $ region_width s) str
        -- Convert empty lines into hFills because otherwise Vty will
        -- collapse them.
        convert [] = hFill attr ' ' 1
        convert line = text attr line
        toRender = take (fromEnum $ region_height s) ws
    in renderMany Vertical $ if null toRender
                             then [render (hFill attr ' ' 1) s]
                             else toRender