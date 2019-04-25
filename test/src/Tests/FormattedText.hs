module Tests.FormattedText where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Applicative
import qualified Data.Text as T

import Graphics.Vty hiding (regionHeight, regionWidth)
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

import Tests.Util

import Tests.Instances ()

sz :: DisplayRegion
sz = (100, 100)

textHeight :: Property
textHeight =
    monadicIO $ forAllM textString $ \str -> do
      w <- run $ plainText str
      img <- run $ render w sz defaultContext
      if regionHeight sz == 0 then
          return $ imageHeight img == 0 else
          return $ imageHeight img == (toEnum $ numNewlines str + 1)

textImageSize :: Property
textImageSize =
    monadicIO $ forAllM textString $ \str ->
        sizeTest (plainText str)

textSetText :: Property
textSetText =
    monadicIO $ forAllM textString $ \s1 ->
      forAllM textString $ \s2 -> do
        w1 <- run $ plainText s1
        w2 <- run $ plainText s2
        img1 <- run $ render w1 sz defaultContext
        img2 <- run $ render w2 sz defaultContext
        run $ setText w2 s1
        img3 <- run $ render w2 sz defaultContext
        return $ img1 == img3 && img1 /= img2

textString :: Gen T.Text
textString = T.pack <$> (listOf $ oneof [ pure 'a'
                                        , pure '\n'
                                        , pure ' '
                                        , pure '台'
                                        ])

tests :: [Property]
tests = [ label "text: newlines rendered correctly" textHeight
        , label "text: image size" textImageSize
        , label "text: setText works" textSetText
        ]
