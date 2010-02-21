{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Tokenize where

import Data.Char ( isPrint )
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

lineGen :: Gen [Token ()]
lineGen = listOf1 arbitrary

stringGen :: Gen String
stringGen = listOf (arbitrary `suchThat` (\c -> isPrint c))

checkToken :: Token a -> Bool
checkToken (Whitespace s _) = all (`elem` " \t") s
checkToken (Token s _) = all (not . (`elem` " \t")) s

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (a:as) = count f as + if f a then 1 else 0

numNewlines :: String -> Int
numNewlines = count (== '\n')

tests :: [Property]
tests = [ label "tokenizeConsistency" $ property $ forAll tokenGen $
                    \ts -> serialize ts == (serialize $ tokenize (serialize ts) ())
        , label "tokenizeContents" $ property $ forAll stringGen $
                    \s -> all (all checkToken) $ tokenize s undefined
        , label "tokenizeNewlines" $ property $ forAll stringGen $
                    \s -> numNewlines s + 1 == (length $ tokenize s undefined)
        , label "truncLine" $ property $ forAll lineGen $
                    \ts -> forAll (arbitrary :: Gen (Positive Int)) $
                    \width -> length (truncLine (fromIntegral width) ts) <=
                              (fromIntegral width)
        -- wrapping: a single line wrapped should always result in
        -- lines that are no greater than the wrapping width, unless
        -- they have a single token.
        , label "wrapLine" $ property $ forAll lineGen $
                    \ts -> forAll (choose (0, length ts + 1)) $
                    \width -> let ls = wrapLine w ts
                                  w = fromIntegral width
                                  f l = length (serialize [l]) <= w || length l == 1
                              in all f ls
        ]
