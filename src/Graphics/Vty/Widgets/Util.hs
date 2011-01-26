module Graphics.Vty.Widgets.Util
    ( on
    , fgColor
    , bgColor
    , style
    , mergeAttr
    , mergeAttrs
    , withWidth
    , withHeight
    )
where

import Data.Word
    ( Word
    )
import Graphics.Vty
    ( Color
    , Attr(..)
    , Style
    , MaybeDefault(..)
    , DisplayRegion(..)
    , def_attr
    , with_style
    , with_back_color
    , with_fore_color
    )

on :: Color -> Color -> Attr
on a b = def_attr `with_back_color` b `with_fore_color` a

fgColor :: Color -> Attr
fgColor = (def_attr `with_fore_color`)

bgColor :: Color -> Attr
bgColor = (def_attr `with_back_color`)

style :: Style -> Attr
style = (def_attr `with_style`)

-- Left-most attribute's fields take precedence.
mergeAttr :: Attr -> Attr -> Attr
mergeAttr a b =
    let b1 = case attr_style a of
               SetTo v -> b `with_style` v
               _ -> b
        b2 = case attr_fore_color a of
               SetTo v -> b1 `with_fore_color` v
               _ -> b1
        b3 = case attr_back_color a of
               SetTo v -> b2 `with_back_color` v
               _ -> b2
    in b3

mergeAttrs :: [Attr] -> Attr
mergeAttrs attrs = foldr mergeAttr def_attr attrs

-- |Modify the width component of a 'DisplayRegion'.
withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

-- |Modify the height component of a 'DisplayRegion'.
withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h
