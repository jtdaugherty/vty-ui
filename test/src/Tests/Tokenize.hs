{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Tokenize where

import Data.Char ( isPrint )
import Control.Applicative ( (<$>), (<*>), pure )
import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Text.Trans.Tokenize

import Tests.Util

lineGen :: Gen [Token ()]
lineGen = listOf1 $ oneof [wsgen, strgen]
    where
      strgen = do
        s <- stringGen
        return $ S s ()

      wsgen = do
        s <- listOf1 $ elements " \t"
        return $ WS s ()

stringGen :: Gen String
stringGen = listOf1 charGen

charGen :: Gen Char
charGen = arbitrary `suchThat` (\c -> isPrint c)

tests :: [Property]
tests = [ label "tokenize and serialize work" $ property $ forAll stringGen $
                    \s -> (serialize $ tokenize s ()) == s

        , label "truncLine leaves short lines unchanged" $ property $ forAll lineGen $
                    \ts -> ts == truncLine (length $ serialize (TS (map T ts))) ts

        -- Bound the truncation width at twice the size of the input
        -- since huge cases are silly.
        , label "truncLine truncates long lines" $ property $ forAll lineGen $
                    \ts -> forAll (choose (0, 2 * (length $ serialize (TS (map T ts))))) $
                           \width -> length (serialize $ (TS (map T $ truncLine width ts))) <= width

        , label "wrapStream does the right thing with whitespace" $ property $
                and [ wrapStream 5 (TS [T (S "foo" ()), T (WS " " ()), T (S "bar" ())]) ==
                                     (TS [ T (S "foo" ())
                                         , T (WS " " ())
                                         , NL
                                         , T (S "bar" ())
                                         ])

                    , wrapStream 5 (TS [ T (S "foo" ()), T (WS " " ()), T (S "bar" ())
                                       , T (WS " " ()), T (S "baz" ())]) ==
                                     (TS [ T (S "foo" ())
                                         , T (WS " " ())
                                         , NL
                                         , T (S "bar" ())
                                         , T (WS " " ())
                                         , NL
                                         , T (S "baz" ())
                                         ])

                    , wrapStream 3 (TS [ T (S "foo" ()), T (WS " " ()), T (S "bar" ())]) ==
                                     (TS [ T (S "foo" ())
                                         , NL
                                         , T (S "bar" ())
                                         ])

                    , wrapStream 6 (TS [ T (S "foo" ()), T (WS " " ()), T (S "bar" ())]) ==
                                     (TS [ T (S "foo" ())
                                         , T (WS " " ())
                                         , NL
                                         , T (S "bar" ())
                                         ])

                    , wrapStream 7 (TS [ T (S "foo" ()), T (WS " " ()), T (S "bar" ())]) ==
                                     (TS [ T (S "foo" ()), T (WS " " ()), T (S "bar" ())])

                    , wrapStream 3 (TS [T (WS " " ())]) == TS [T (WS " " ())]
                    ]

        , label "wrapStream preserves newlines" $ property $
                let ts = TS [T (S "foo" ()), NL, T (S "bar" ())]
                in wrapStream 3 ts == ts

        , label "wrapStream does the right thing if unable to wrap" $
                property $ and [ wrapStream 2 (TS [T (S "FOO" ())]) == TS [T (S "FOO" ())]
                               , wrapStream 2 (TS [T (S "FOO" ()), (T (S "BAR" ()))]) ==
                                                (TS [T (S "FOO" ()), NL, (T (S "BAR" ()))])
                               ]

        -- Each line must be wrapped and be no longer than the
        -- wrapping width OR it must be one token in length, assuming
        -- that token was longer than the wrapping width and couldn't
        -- be broken up.
        , label "wrapLine wraps long lines when possible" $
                property $ forAll lineGen $
                    \ts -> forAll (choose ((length $ tokenStr $ ts !! 0), (length $ serialize (TS $ map T ts)) - 1)) $
                           \width -> let lines = findLines new
                                         TS new = wrapStream width $ TS $ map T ts
                                     in all (\l -> (length (serialize $ TS l)) <= width || (length l == 1)) lines
        ]
