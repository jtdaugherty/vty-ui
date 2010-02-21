{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Instances where

import Test.QuickCheck
import Control.Applicative ( (<*>), (<$>), pure )
import Data.Word ( Word8 )

import Graphics.Vty

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
