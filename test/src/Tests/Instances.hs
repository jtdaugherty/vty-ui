{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Instances where

import Test.QuickCheck
import Control.Applicative ( (<*>), (<$>), pure )

import Graphics.Vty

instance (Show a, Arbitrary a, Eq a) => Arbitrary (MaybeDefault a) where
    arbitrary = oneof [ pure Default
                      , pure KeepCurrent
                      , SetTo <$> arbitrary
                      ]

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
