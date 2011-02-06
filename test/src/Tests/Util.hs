module Tests.Util where

import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tests.Instances ()

imageSize :: Image -> DisplayRegion -> Bool
imageSize img sz =
    image_width img <= region_width sz && image_height img <= region_height sz

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (a:as) = count f as + if f a then 1 else 0

numNewlines :: String -> Int
numNewlines = count (== '\n')

sizeTest :: (Show a) => IO (Widget a) -> PropertyM IO Bool
sizeTest mkWidget =
    forAllM arbitrary $ \sz -> do
      w <- run mkWidget
      img <- run $ render w sz defaultContext
      if region_height sz == 0 || region_width sz == 0 then
          return $ image_height img == 0 && image_width img == 0 else
          return $ image_width img <= region_width sz &&
                 image_height img <= region_height sz
