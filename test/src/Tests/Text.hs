module Tests.Text where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Char ( isPrint )

import Graphics.Vty
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Core

import Tests.Util

import Tests.Instances ()

textSize :: Property
textSize =
    monadicIO $ forAllM textString $ \str ->
        forAllM arbitrary $ \sz -> do
          w <- run $ simpleText str
          img <- run $ render w sz defaultContext
          if null str || region_height sz == 0 || region_width sz == 0 then
              return $ image_height img == 0 && image_width img == 0 else
              return $ image_width img <= (toEnum $ length str) &&
                     image_height img == (toEnum $ numNewlines str + 1)

textString :: Gen String
textString = listOf (arbitrary `suchThat` (\c -> isPrint c))

tests :: [Property]
tests = [ label "textSize" textSize
        ]