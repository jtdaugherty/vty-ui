{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Tokenize where

import Control.Applicative
import Data.Char ( isPrint )
import Test.QuickCheck
import Tests.Util
import Text.Trans.Tokenize
import Graphics.Vty.Widgets.Util
import qualified Data.Text as T

lineGen :: Gen [Token ()]
lineGen = listOf1 $ oneof [wsgen, strgen]
    where
      strgen = do
        s <- stringGen
        return $ S (T.pack s) ()

      wsgen = do
        s <- listOf1 $ elements " \t"
        return $ WS (T.pack s) ()

stringGen :: Gen String
stringGen = listOf1 charGen

textGen :: Gen T.Text
textGen = T.pack <$> stringGen

charGen :: Gen Char
charGen = oneof [ arbitrary `suchThat` (\c -> isPrint c)
                , pure '台'
                ]

tests :: [Property]
tests = [ label "tokenize and serialize work" $
                property $ forAll textGen $
                    \s -> (serialize $ tokenize s ()) == s

        , label "truncateLine leaves short lines unchanged" $
                property $ forAll lineGen $
                    \ts -> ts == truncateLine (lineLength ts) ts

        -- Bound the truncation width at twice the size of the input
        -- since huge cases are silly.
        , label "truncateLine truncates long lines" $
                property $ forAll lineGen $
                    \ts -> forAll (choose (0, 2 * (lineLength ts))) $
                           \width -> (lineLength $ truncateLine width ts) <= width

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
                    \ts -> forAll (choose ((textWidth $ tokenStr $ ts !! 0), (lineLength ts - Phys 1))) $
                           \width -> let TS new = wrapStream width $ TS $ T <$> ts
                                         ls = findLines new
                                         check l = (lineLength $ entityToken <$> l) <= width || (length l == 1)
                                     in all check ls
        ]
