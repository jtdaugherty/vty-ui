module Tests.FormattedText where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Applicative

import Graphics.Vty
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Core

import Tests.Util

import Tests.Instances ()

sz :: DisplayRegion
sz = DisplayRegion 100 100

textHeight :: Property
textHeight =
    monadicIO $ forAllM textString $ \str -> do
      w <- run $ plainText str
      img <- run $ render w sz defaultContext
      if region_height sz == 0 then
          return $ image_height img == 0 else
          return $ image_height img == (toEnum $ numNewlines str + 1)

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

textString :: Gen String
textString = listOf $ oneof [ pure 'a'
                            , pure '\n'
                            , pure ' '
                            ]

tests :: [Property]
tests = [ label "text: newlines rendered correctly" textHeight
        , label "text: image size" textImageSize
        , label "text: setText works" textSetText
        ]
