module Graphics.Vty.Widgets.Util
    ( on
    )
where

import Graphics.Vty
    ( Color
    , Attr
    , def_attr
    , with_back_color
    , with_fore_color
    )

on :: Color -> Color -> Attr
on a b = def_attr `with_back_color` b `with_fore_color` a