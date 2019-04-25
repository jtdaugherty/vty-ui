module Tests.Util where

import Control.Applicative
import qualified Data.Text as T
import Graphics.Vty hiding (regionHeight, regionWidth)
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util
import Text.Trans.Tokenize
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tests.Instances ()

imageSize :: Image -> DisplayRegion -> Bool
imageSize img sz =
    imageWidth img <= regionWidth sz && imageHeight img <= regionHeight sz

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (a:as) = count f as + if f a then 1 else 0

numNewlines :: T.Text -> Int
numNewlines = count (== '\n') . T.unpack

sizeGen :: Gen DisplayRegion
sizeGen = (,)
          <$> (arbitrary `suchThat` (>= 0))
          <*> (arbitrary `suchThat` (>= 0))

sizeTest :: (Show a) => IO (Widget a) -> PropertyM IO Bool
sizeTest mkWidget =
    forAllM sizeGen $ \sz -> do
      w <- run mkWidget
      img <- run $ render w sz defaultContext
      if regionHeight sz == 0 || regionWidth sz == 0 then
          return $ imageHeight img == 0 && imageWidth img == 0 else
          return $ imageWidth img <= regionWidth sz &&
                 imageHeight img <= regionHeight sz

lineLength :: [Token a] -> Phys
lineLength = sum . (textWidth <$>) . (tokenStr <$>)
