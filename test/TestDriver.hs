{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import System.Exit ( exitFailure, exitSuccess )

import Data.Word ( Word8 )
import Data.Char ( isPrint )
import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Applicative ( (<$>), (<*>), pure )

import Graphics.Vty
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Rendering

instance (Arbitrary a, Eq a) => Arbitrary (MaybeDefault a) where
    arbitrary = oneof [ pure Default
                      , pure KeepCurrent
                      , SetTo <$> arbitrary
                      ]

instance Arbitrary Word8 where
    arbitrary = toEnum <$> choose (0, 255)

instance Arbitrary Color where
    arbitrary = oneof [ ISOColor <$> arbitrary
                      , Color240 <$> arbitrary
                      ]

instance Arbitrary Attr where
    arbitrary = Attr <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DisplayRegion where
    arbitrary = DisplayRegion <$> coord <*> coord
        where
          coord = sized $ \n -> fromIntegral <$> choose (0, n)

toImage :: DisplayRegion -> Widget -> Image
toImage sz w = fst $ mkImageSize upperLeft sz w
    where upperLeft = DisplayRegion 0 0

textSize :: Property
textSize =
    property $ forAll textString $ \str attr sz ->
        let img = toImage sz $ text attr str
        in
          if null str || region_height sz == 0 || region_width sz == 0
          then image_height img == 0 && image_width img == 0
          else image_width img <= (toEnum $ length str) && image_height img == 1

imageSize :: Widget -> DisplayRegion -> Bool
imageSize w sz =
    image_width img <= region_width sz && image_height img <= region_height sz
        where
          img = toImage sz w

textString :: Gen String
textString = listOf (arbitrary `suchThat` (\c -> isPrint c && c /= '\n'))

tests :: [Property]
tests = [ textSize
        , property $ forAll textString $ \str attr -> imageSize (text attr str)
        ]

main :: IO ()
main = do
  results <- mapM quickCheckResult tests
  if all isSuccess results then
      exitSuccess else
      exitFailure
