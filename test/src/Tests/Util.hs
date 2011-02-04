module Tests.Util where

import Graphics.Vty

imageSize :: Image -> DisplayRegion -> Bool
imageSize img sz =
    image_width img <= region_width sz && image_height img <= region_height sz

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (a:as) = count f as + if f a then 1 else 0

numNewlines :: String -> Int
numNewlines = count (== '\n')
