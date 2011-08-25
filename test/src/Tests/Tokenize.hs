{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Tokenize where

import Data.Char ( isPrint )
import Control.Applicative ( (<$>), (<*>), pure )
import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Text.Trans.Tokenize

import Tests.Util

serialize :: [(String, a)] -> String
serialize [] = ""
serialize ((s, _):ss) = s ++ serialize ss

lineGen :: Gen [(String, ())]
lineGen = listOf1 $ stringGen >*< arbitrary

nonEmptyLineGen :: Gen [(String, ())]
nonEmptyLineGen = listOf1 $ (listOf1 charGen) >*< arbitrary

wrapLineGen :: Gen [(String, ())]
wrapLineGen = do
  t <- token
  ts <- listOf1 token
  return $ t:ts
    where
      token = str >*< arbitrary
      str = do
        c <- arbitrary
        cs <- listOf1 charGen
        return $ c:cs

stringGen :: Gen String
stringGen = listOf charGen

charGen :: Gen Char
charGen = arbitrary `suchThat` (\c -> isPrint c)

tests :: [Property]
tests = [ label "truncLine leaves short lines unchanged" $ property $ forAll lineGen $
                    \ts -> ts == truncLine (length $ serialize ts) ts

        -- Bound the truncation width at twice the size of the input
        -- since huge cases are silly.
        , label "truncLine truncates long lines" $ property $ forAll nonEmptyLineGen $
                    \ts -> forAll (choose (0, 2 * (length $ serialize ts))) $
                           \width -> length (serialize $ truncLine width ts) <= width

        , label "wrapLine leaves short lines unchanged" $ property $ forAll nonEmptyLineGen $
                    \ts -> forAll (arbitrary `suchThat` (> (length $ serialize ts))) $
                           \width -> [ts] == wrapLine width ts

        , label "wrapLine leaves equal lines unchanged" $ property $ forAll nonEmptyLineGen $
                    \ts -> [ts] == wrapLine (length $ serialize ts) ts

        , label "wrapLine does nothing if unable to wrap" $
                property $ and [ wrapLine 2 [("FOO", ())] == [[("FOO", ())]]
                               , wrapLine 1 [("FOO", ())] == [[("FOO", ())]]
                               , wrapLine 2 [("FOO", ()), ("BAR", ())] ==
                                              [[("FOO", ())], [("BAR", ())]]
                               ]

        -- Each line must be wrapped and be no longer than the
        -- wrapping width OR it must be one token in length, assuming
        -- that token was longer than the wrapping width and couldn't
        -- be broken up.
        --
        -- XXX note that this is in need of some improvement, since we
        -- *should* be breaking tokens up on whitespace when possible
        -- but the current implementation does not do that.
        , label "wrapLine wraps long lines when possible" $
                property $ forAll wrapLineGen $
                    \ts -> forAll (choose ((length $ fst $ ts !! 0), (length $ serialize ts) - 1)) $
                           \width -> let lines = wrapLine width ts
                                     in all (\l -> (length $ serialize l) <= width || (length l == 1)) lines
        ]
