{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Text where

import Control.Applicative ( (<$>), (<*>), pure )
import Test.QuickCheck
import Data.Char ( isPrint )

import Graphics.Vty
import Graphics.Vty.Widgets.Text
import Text.Trans.Tokenize

import Tests.Util
import Tests.Instances ()

instance (Arbitrary a) => Arbitrary (Token a) where
    arbitrary = oneof [ Whitespace <$> ws <*> arbitrary
                      , Token <$> s <*> arbitrary
                      ]
        where
          ws = oneof [ pure " ", pure "\t" ]
          s = replicate <$> choose (1, 10) <*> pure 'a'

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

tokenGen :: Gen [[Token ()]]
tokenGen = listOf $ listOf arbitrary

tests :: [Property]
tests = [ label "textSize" textSize
        , label "imageSize" $ property $ forAll textString $
                    \str attr -> imageSize (simpleText attr str)
        -- Round-trip property for token serialization and string
        -- tokenization.
        , label "tokenizeRoundTrip" $ property $ forAll tokenGen $
                    \ts -> serialize ts == (serialize $ tokenize (serialize ts) ())
        ]