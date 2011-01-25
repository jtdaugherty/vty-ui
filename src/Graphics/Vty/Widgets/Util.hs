module Graphics.Vty.Widgets.Util
    ( on
    , fgColor
    , bgColor
    , mergeAttr
    , mergeAttrs
    )
where

import Graphics.Vty
    ( Color
    , Attr(..)
    , MaybeDefault(..)
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
mergeAttrs attrs = foldl mergeAttr def_attr attrs