module Tests.Text where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Applicative

import Graphics.Vty
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Core

import Tests.Util

import Tests.Instances ()

textHeight :: Property
textHeight =
    monadicIO $ forAllM textString $ \str -> do
      let sz = DisplayRegion 100 100
      w <- run $ simpleText str
      img <- run $ render w sz defaultContext
      if region_height sz == 0 then
          return $ image_height img == 0 else
          return $ image_height img == (toEnum $ numNewlines str + 1)

textImageSize :: Property
textImageSize =
    monadicIO $ forAllM textString $ \str ->
        sizeTest (simpleText str)

textString :: Gen String
textString = listOf $ oneof [ pure 'a'
                            , pure '\n'
                            , pure ' '
                            ]

tests :: [Property]
tests = [ label "text: newlines rendered correctly" textHeight
        , label "text: image size" textImageSize
        ]