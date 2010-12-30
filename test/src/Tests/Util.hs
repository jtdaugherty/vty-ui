module Tests.Util where

import Graphics.Vty
import Graphics.Vty.Widgets.Rendering

imageSize :: Widget a -> DisplayRegion -> Bool
imageSize w sz =
    image_width img <= region_width sz && image_height img <= region_height sz
        where
          img = fst $ render w sz