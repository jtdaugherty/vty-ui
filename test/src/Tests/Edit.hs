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

wideCharStrings :: Gen String
wideCharStrings = listOf1 $ pure '台'

splitLineLength :: Property
splitLineLength =
    property $ forAll stringGen $ \s ->
        forAll (choose (Phys 0, strWidth s + Phys 1)) $ \width ->
            let (l, _, _) = splitLine width s
            in strWidth l <= width

splitLineWideChar :: Property
splitLineWideChar =
    property $ forAll wideCharStrings $ \s ->
        forAll (choose (Phys 0, strWidth s - Phys 1) `suchThat` (>= Phys 0)) $ \width ->
            let (l, r, i) = splitLine width s
            in if width `mod` 2 == 0
               then and [ i == False
                        , length l == 0 || last l /= indicatorChar
                        , length l == 0 || head r /= indicatorChar
                        ]
               else and [ i == True
                        , length l == 0 || last l == indicatorChar
                        , length l == 0 || head r == indicatorChar
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
tests = [ label "splitLine guarantees the left string length" splitLineLength
        , label "splitLine handles wide characters correctly" splitLineWideChar
        , label "cropLine guarantees the result length" cropLineLength
        , label "cropLine adds indicators correctly" cropLineIndicators
        ]