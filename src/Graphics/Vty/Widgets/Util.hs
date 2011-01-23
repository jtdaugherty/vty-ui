module Graphics.Vty.Widgets.Util
    ( on
    , alt
    )
where

import Control.Applicative
    ( Alternative
    , (<|>)
    )
import Graphics.Vty
    ( Color
    , Attr
    , def_attr
    , with_back_color
    , with_fore_color
    )

on :: Color -> Color -> Attr
on a b = def_attr `with_back_color` b `with_fore_color` a

-- Pretty ugly that we have to do this, but Vty defines <|> to mean
-- something else.
alt :: (Alternative f) => f a -> f a -> f a
alt = (<|>)