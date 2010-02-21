{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Tokenize where

import Control.Applicative ( (<$>), (<*>), pure )
import Test.QuickCheck

import Text.Trans.Tokenize

instance (Arbitrary a) => Arbitrary (Token a) where
    arbitrary = oneof [ Whitespace <$> ws <*> arbitrary
                      , Token <$> s <*> arbitrary
                      ]
        where
          ws = oneof [ pure " ", pure "\t" ]
          s = replicate <$> choose (1, 10) <*> pure 'a'

tokenGen :: Gen [[Token ()]]
tokenGen = listOf $ listOf arbitrary

tests :: [Property]
tests = [ label "tokenizeConsistency" $ property $ forAll tokenGen $
                    \ts -> serialize ts == (serialize $ tokenize (serialize ts) ())
        ]
