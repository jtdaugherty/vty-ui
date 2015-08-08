{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Instances where

import Test.QuickCheck
import System.Random
import Control.Applicative ( (<*>), (<$>), pure )

import Graphics.Vty
import Graphics.Vty.Widgets.Util

instance (Show a, Read a, Arbitrary a, Eq a) => Arbitrary (MaybeDefault a) where
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

instance Random Phys where
    random g =
        let (val, g') = random g
        in (Phys val, g')

    randomR (Phys a, Phys b) g =
        let (val, g') = randomR (a, b) g
        in (Phys val, g')

instance Arbitrary Phys where
    arbitrary = Phys <$> arbitrary
