module Tests.Util where

import Graphics.Vty

imageSize :: Image -> DisplayRegion -> Bool
imageSize img sz =
    image_width img <= region_width sz && image_height img <= region_height sz
