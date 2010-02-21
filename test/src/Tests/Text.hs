module Tests.Text where

import Test.QuickCheck
import Data.Char ( isPrint )

import Graphics.Vty
import Graphics.Vty.Widgets.Text

import Tests.Util
import Tests.Instances ()

textSize :: Property
textSize =
    property $ forAll textString $ \str attr sz ->
        let img = toImage sz $ simpleText attr str
        in
          if null str || region_height sz == 0 || region_width sz == 0
          then image_height img == 0 && image_width img == 0
          else image_width img <= (toEnum $ length str) && image_height img <= 1

textString :: Gen String
textString = listOf (arbitrary `suchThat` (\c -> isPrint c && c /= '\n'))

tests :: [Property]
tests = [ label "textSize" textSize
        , label "imageSize" $ property $ forAll textString $
                    \str attr -> imageSize (simpleText attr str)
        ]