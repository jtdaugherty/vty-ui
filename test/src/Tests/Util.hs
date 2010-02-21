module Tests.Util where

import Graphics.Vty
import Graphics.Vty.Widgets.Rendering

toImage :: DisplayRegion -> Widget -> Image
toImage sz w = fst $ mkImageSize upperLeft sz w
    where upperLeft = DisplayRegion 0 0

imageSize :: Widget -> DisplayRegion -> Bool
imageSize w sz =
    image_width img <= region_width sz && image_height img <= region_height sz
        where
          img = toImage sz w