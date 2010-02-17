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
    ( Token(..)
    , tokenize
    , serialize
    , withAnnotation
    , trunc
    , splitWith
    , isNewline
    , isWhitespace
    , splitLines
    , wrapLine
    )
where

import Data.List
    ( inits
    , intersperse
    , splitAt
    )

-- |The type of textual tokens.  Tokens have an "annotation" type,
-- which is the type of values that can be used to annotate tokens
-- (e.g., position in a file, visual attributes, etc.).
data Token a = Newline a
             -- ^A line break.
             | Whitespace String a
             -- ^A sequence of whitespace characters.
             | Token String a
             -- ^A non-whitespace token.
               deriving (Show, Eq)

-- |General splitter function; given a list and a predicate, split the
-- list into sublists wherever the predicate matches, discarding the
-- matching elements.
splitWith :: (Eq a) => [a] -> (a -> Bool) -> [[a]]
splitWith [] _ = []
splitWith es f = if null rest
                 then [first]
                 else first : splitWith (tail rest) f
    where
      (first, rest) = break f es

wsChars :: [Char]
wsChars = [' ', '\t']

isWs :: Char -> Bool
isWs = (`elem` wsChars)

-- |Tokenize a string using a default annotation value.
tokenize :: String -> a -> [Token a]
tokenize [] _ = []
tokenize ('\n':rest) a = Newline a : tokenize rest a
tokenize s@(c:_) a | isWs c = Whitespace ws a : tokenize rest a
    where
      (ws, rest) = break (not . isWs) s
tokenize s a = Token t a : tokenize rest a
    where
      (t, rest) = break (\c -> isWs c || c == '\n') s

-- |Serialize tokens to an underlying string representation,
-- discarding annotations.
serialize :: [Token a] -> String
serialize [] = []
serialize (Newline _:rest) = "\n" ++ serialize rest
serialize (Whitespace s _:rest) = s ++ serialize rest
serialize (Token s _:rest) = s ++ serialize rest

-- |Replace a token's annotation.
withAnnotation :: Token a -> a -> Token a
withAnnotation (Newline _) b = Newline b
withAnnotation (Whitespace s _) b = Whitespace s b
withAnnotation (Token s _) b = Token s b

-- |Is the token a line break?
isNewline :: Token a -> Bool
isNewline (Newline _) = True
isNewline _ = False

-- |Is the token whitespace?
isWhitespace :: Token a -> Bool
isWhitespace (Whitespace _ _) = True
isWhitespace _ = False

-- |Split a list of tokens at Newlines, such that the returned token
-- lists do not contain Newlines.
splitLines :: (Eq a) => [Token a] -> [[Token a]]
splitLines ts = splitWith ts isNewline

-- |Truncate a token stream at a given column width.
trunc :: (Eq a) => a -> [Token a] -> Int -> [Token a]
trunc def ts width = concat $ intersperse [Newline def] newLines
    where
      newLines = map (\l -> truncLine width l) $ splitLines ts

-- |Given a list of tokens, truncate the list so that its underlying
-- string representation does not exceed the specified column width.
-- Note that this does not truncate /within/ a token; it merely
-- returns the largest sublist of tokens that has the required length.
truncLine :: Int -> [Token a] -> [Token a]
truncLine width ts = take (length $ head passing) ts
    where
      lengths = map len ts
      cases = reverse $ inits lengths
      passing = dropWhile (\c -> sum c > width) cases

len :: Token a -> Int
len (Newline _) = 0
len (Whitespace s _) = length s
len (Token s _) = length s

-- XXX This assumes that the input token list will not contain
-- newlines (i.e., that splitLines has already been called).
-- |Given a list of tokens without Newlines, (potentially) wrap the
-- list to the specified column width, using the specified default
-- annotation.
wrapLine :: (Eq a) => a -> Int -> [Token a] -> [[Token a]]
wrapLine _ _ [] = []
wrapLine def width ts =
    -- If there were no passing cases, that means the line can't be
    -- wrapped so just return it as-is (e.g., one long unbroken
    -- string).  Otherwise, package up the acceptable tokens and
    -- continue wrapping.
    if null passing
    then [ts]
    else these : wrapLine def width those
    where
      lengths = map len ts
      cases = reverse $ inits lengths
      passing = dropWhile (\c -> sum c > width) cases
      numTokens = length $ head passing
      (these, those') = splitAt numTokens ts
      those = dropWhile isWhitespace those'
