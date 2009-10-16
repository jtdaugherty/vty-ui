-- |This module provides a widget which automatically wraps text in
-- the available space.  To create a 'WrappedText', see 'wrappedText'.
module Graphics.Vty.Widgets.WrappedText
    ( WrappedText
    , wrappedText
    )
where

import Text.Trans.Wrap ( wrap )
import Graphics.Vty
    ( Attr
    , region_width
    , region_height
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Orientation(..)
    , renderMany
    )
import Graphics.Vty.Widgets.Base
    ( text
    , hFill
    , anyWidget
    )

-- |A text widget which automatically wraps its contents to fit in the
-- available space.
data WrappedText = WrappedText Attr String

-- |Create a 'WrappedText' widget from the specified attribute and
-- text.
wrappedText :: Attr -> String -> WrappedText
wrappedText = WrappedText

instance Widget WrappedText where
    growHorizontal _ = True
    growVertical _ = False

    primaryAttribute (WrappedText att _) = att

    withAttribute (WrappedText _ t) att = WrappedText att t

    render s (WrappedText attr str) =
        let ws = map (render s . convert) $ lines wrapped
            wrapped = wrap (fromEnum $ region_width s) str
            -- Convert empty lines into hFills because otherwise Vty
            -- will collapse them.
            convert [] = anyWidget $ hFill attr ' ' 1
            convert line = anyWidget $ text attr line
        in renderMany Vertical $ take (fromEnum $ region_height s) ws