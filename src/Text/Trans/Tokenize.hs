{-# LANGUAGE CPP #-}
-- |This module provides a tokenization API for text strings.  The
-- idea is that if you want to make structural or representational
-- changes to a text stream, it needs to be split up into reasonable
-- tokens first, with structural properties intact.  This is
-- accomplished by the 'Token' type.  To get started, call 'tokenize'
-- to turn your String into tokens; then you can use the other
-- operations provided here to make structural or representational
-- changes.  To serialize a token list to its underlying string form,
-- use 'serialize'.
module Text.Trans.Tokenize
    ( tokenize
    , serialize
    , withAnnotation
    , truncLine
    , isWhitespace
    , wrapLine
#ifdef TESTING
    , Token(..)
#else
    , Token
    , tokenString
    , tokenAnnotation
#endif
    )
where

import Data.List
    ( inits
    , intercalate
    )

-- |The type of textual tokens.  Tokens have an "annotation" type,
-- which is the type of values that can be used to annotate tokens
-- (e.g., position in a file, visual attributes, etc.).
data Token a = Whitespace { tokenString :: String
                          , tokenAnnotation :: a
                          }
             | Token { tokenString :: String
                     , tokenAnnotation :: a
                     }
               deriving (Show, Eq)

-- |General splitter function; given a list and a predicate, split the
-- list into sublists wherever the predicate matches, discarding the
-- matching elements.
splitWith :: (Eq a) => [a] -> (a -> Bool) -> [[a]]
splitWith [] _ = []
splitWith es f = if null rest
                 then [first]
                 else if length rest == 1 && (f $ head rest)
                      then first : [[]]
                      else first : splitWith (tail rest) f
    where
      (first, rest) = break f es

wsChars :: [Char]
wsChars = [' ', '\t']

isWs :: Char -> Bool
isWs = (`elem` wsChars)

-- |Tokenize a string using a default annotation value.
tokenize :: String -> a -> [[Token a]]
tokenize [] _ = [[]]
tokenize s def = map (tokenize' def) $ splitWith s (== '\n')

tokenize' :: a -> String -> [Token a]
tokenize' _ [] = []
tokenize' a s@(c:_) | isWs c = Whitespace ws a : tokenize' a rest
    where
      (ws, rest) = break (not . isWs) s
tokenize' a s = Token t a : tokenize' a rest
    where
      (t, rest) = break (\c -> isWs c || c == '\n') s

-- |Serialize tokens to an underlying string representation,
-- discarding annotations.
serialize :: [[Token a]] -> String
serialize ls = intercalate "\n" $ map (concatMap tokenString) ls

-- |Replace a token's annotation.
withAnnotation :: Token a -> a -> Token a
withAnnotation (Whitespace s _) b = Whitespace s b
withAnnotation (Token s _) b = Token s b

-- |Is the token whitespace?
isWhitespace :: Token a -> Bool
isWhitespace (Whitespace _ _) = True
isWhitespace _ = False

-- |Given a list of tokens, truncate the list so that its underlying
-- string representation does not exceed the specified column width.
truncLine :: Int -> [Token a] -> [Token a]
truncLine width ts =
    -- If we are returning all tokens, we didn't have to do any
    -- truncation.  But if we *did* have to truncate, return exactly
    -- 'width' characters' worth of tokens by constructing a new final
    -- token with the same attribute data.
    if length tokens == length ts
                     then tokens
                     else tokens ++ [lastToken]
    where
      lengths = map (length . tokenString) ts
      cases = reverse $ inits lengths
      remaining = dropWhile ((> width) . sum) cases
      tokens = take (length $ head remaining) ts
      truncLength = sum $ head remaining

      lastTokenBasis = ts !! (length tokens)
      lastToken = lastTokenBasis {
                    tokenString = take (width - truncLength) (tokenString lastTokenBasis)
                  }

-- |Given a list of tokens without Newlines, (potentially) wrap the
-- list to the specified column width.
wrapLine :: Int -> [Token a] -> [[Token a]]
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
      lengths = map (length . tokenString) ts
      cases = reverse $ inits lengths
      passing = dropWhile (\c -> sum c > width) cases
      numTokens = length $ head passing
      (these, those') = splitAt numTokens ts
      those = dropWhile isWhitespace those'
