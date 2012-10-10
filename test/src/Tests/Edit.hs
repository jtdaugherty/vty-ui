module Tests.Edit where

import Control.Applicative
import Data.Char ( isPrint )
import Test.QuickCheck
import Tests.Instances ()
import qualified Data.Text as T

import Graphics.Vty.Widgets.Edit
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.TextClip

stringGen :: Gen String
stringGen = listOf1 charGen

textGen :: Gen T.Text
textGen = T.pack <$> (listOf $ oneof [ pure 'a'
                                     , pure ' '
                                     , pure '台'
                                     ])

charGen :: Gen Char
charGen = oneof [ arbitrary `suchThat` (\c -> isPrint c)
                , pure '台'
                ]

doClippingTest :: Property
doClippingTest =
    property $ forAll textGen $ \t ->
        forAll (choose (Phys 0, textWidth t + Phys 10)) $ \leftCrop ->
        forAll (choose (Phys 0, textWidth t + Phys 10)) $ \resWidth ->
            let rect = ClipRect { clipLeft = leftCrop
                                , clipWidth = resWidth
                                , clipTop = 0
                                , clipHeight = 1
                                }
                [(_, ls, rs)] = clip2d rect [t]
                [res] = doClipping [t] rect
            in strWidth res == 0
                   || and [ not ls || (ls && head res == indicatorChar)
                          , not rs || (rs && last res == indicatorChar)
                          ]

tests :: [Property]
tests = [ label "doClipping clips and adds indicators" doClippingTest
        ]