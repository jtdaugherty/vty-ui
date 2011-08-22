{-# LANGUAGE CPP #-}
-- |This module provides a tokenization API for text strings.  The
-- idea is that if you want to make structural or representational
-- changes to a text stream, it needs to be split up into reasonable
-- tokens first, with structural properties intact.  This is
-- accomplished by the 'Token' type.  To get started, call 'tokenize'
-- to turn your String into tokens; then you can use the other
-- operations provided here to make structural or representational
-- changes.
module Text.Trans.Tokenize
    ( truncLine
    , isWhitespace
    , wrapLine
    )
where

import Data.List
    ( inits
    )

wsChars :: [Char]
wsChars = [' ', '\t']

isWs :: Char -> Bool
isWs = (`elem` wsChars)

-- |Is the token whitespace?
isWhitespace :: String -> Bool
isWhitespace = and . map isWs

-- |Given a list of tokens, truncate the list so that its underlying
-- string representation does not exceed the specified column width.
truncLine :: Int -> [(String, a)] -> [(String, a)]
truncLine width ts =
    -- If we are returning all tokens, we didn't have to do any
    -- truncation.  But if we *did* have to truncate, return exactly
    -- 'width' characters' worth of tokens by constructing a new final
    -- token with the same attribute data.
    if length tokens == length ts
                     then tokens
                     else tokens ++ [lastToken]
    where
      lengths = map (length . fst) ts
      cases = reverse $ inits lengths
      remaining = dropWhile ((> width) . sum) cases
      tokens = take (length $ head remaining) ts
      truncLength = sum $ head remaining

      (last_str, last_a) = ts !! (length tokens)
      lastToken = (take (width - truncLength) last_str, last_a)

-- |Given a list of tokens without newlines, (potentially) wrap the
-- list to the specified column width.
wrapLine :: Int -> [(String, a)] -> [[(String, a)]]
wrapLine _ [] = []
wrapLine width ts =
    -- If there were no passing cases, that means the line can't be
    -- wrapped so just return it as-is (e.g., one long unbroken
    -- string).  Otherwise, package up the acceptable tokens and
    -- continue wrapping.
    if null passing
    then [ts]
    else if null these
         then if length those' == 1
              then [those']
              else [head those'] : (wrapLine width $ tail those')
         else these : wrapLine width those
    where
      -- The lengths of all of the tokens in the list.
      lengths = map (length . fst) ts
      -- The sublists of lengths of tokens, so that the longer
      -- sublists come first.
      cases = reverse $ inits lengths
      -- Passing cases are lists of token lengths such that their sum
      -- does not exceed the maximum.
      passing = dropWhile (\c -> sum c > width) cases
      -- numTokens is the number of tokens in the longest passing
      -- case.
      numTokens = length $ head passing
      -- "these" tokens are the tokens whose lengths pass the wrapping
      -- test, and those' are the ones left over.
      (these, those') = splitAt numTokens ts
      -- "those" tokens are the remaining tokens with any leading
      -- whitespace tokens stripped (so the new lines in the resulting
      -- text don't have leading whitespace).
      those = dropWhile (isWhitespace . fst) those'
