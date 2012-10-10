module Tests.TextClip
    ( tests
    )
where

import Control.Applicative
import Test.QuickCheck

import Tests.Instances ()
import qualified Data.Text as T
import Graphics.Vty.Widgets.TextClip
import Graphics.Vty.Widgets.Util

rectGen :: Gen ClipRect
rectGen = ClipRect
          <$> (arbitrary `suchThat` (>= Phys 0))
          <*> (arbitrary `suchThat` (>= Phys 0))
          <*> (arbitrary `suchThat` (>= Phys 0))
          <*> (arbitrary `suchThat` (>= Phys 0))

textGen :: Gen T.Text
textGen = T.pack <$> (listOf $ oneof [ pure 'a'
                                     , pure ' '
                                     , pure '台'
                                     ])

docGen :: Gen [T.Text]
docGen = listOf1 textGen

clip1dWidth :: Property
clip1dWidth =
    forAll rectGen $ \rect ->
        forAll textGen $ \t ->
            let (r, _, _) = clip1d (clipLeft rect) (clipWidth rect) t
            in textWidth r <= clipWidth rect

clipSliceLeft :: Bool
clipSliceLeft =
    let (_, ls, _) = clip1d (Phys 1) (Phys 2) $ T.pack "台台"
    in ls

clipSliceRight :: Bool
clipSliceRight =
    let (_, _, rs) = clip1d (Phys 1) (Phys 2) $ T.pack "台台"
    in rs

clip2dBounds :: Property
clip2dBounds =
    forAll rectGen $ \rect ->
        forAll docGen $ \t ->
            let result = clip2d rect t
                check (r, _, _) = textWidth r <= clipWidth rect
            in all check result && (Phys $ length result) <= clipHeight rect

tests :: [Property]
tests = [ label "clip1d guarantees width bound" clip1dWidth
        , label "clip1d indicates left slices" $ property clipSliceLeft
        , label "clip1d indicates right slices" $ property clipSliceRight
        , label "clip2d guarantees width and height bounds" clip2dBounds
        ]
