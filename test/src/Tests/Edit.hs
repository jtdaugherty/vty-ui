module Tests.Edit where

import Control.Applicative
import Data.Char ( isPrint )
import Test.QuickCheck
import Tests.Instances ()

import Graphics.Vty.Widgets.Edit
import Graphics.Vty.Widgets.Util

stringGen :: Gen String
stringGen = listOf1 charGen

charGen :: Gen Char
charGen = oneof [ arbitrary `suchThat` (\c -> isPrint c)
                , pure '台'
                ]

cropLineLength :: Property
cropLineLength =
    property $ forAll stringGen $ \s ->
        forAll (choose (Phys 0, strWidth s + Phys 10)) $ \leftCrop ->
        forAll (choose (Phys 0, strWidth s + Phys 10)) $ \resWidth ->
            let (res, _, _) = cropLine leftCrop resWidth s
            in if strWidth s - leftCrop > resWidth
               then strWidth res == resWidth
               else strWidth res <= resWidth

cropLineIndicators :: Property
cropLineIndicators =
    property $ forAll stringGen $ \s ->
        forAll (choose (Phys 0, strWidth s + Phys 10)) $ \leftCrop ->
        forAll (choose (Phys 0, strWidth s + Phys 10)) $ \resWidth ->
            let (res, li, ri) = cropLine leftCrop resWidth s
            in strWidth res == 0
                   || and [ not li || (li && head res == indicatorChar)
                          , not ri || (ri && last res == indicatorChar)
                          ]

tests :: [Property]
tests = [ label "cropLine guarantees the result length" cropLineLength
        , label "cropLine adds indicators correctly" cropLineIndicators
        ]