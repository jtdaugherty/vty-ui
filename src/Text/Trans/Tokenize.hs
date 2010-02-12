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

data Token a = Newline a
             | Whitespace String a
             | Token String a
               deriving (Show, Eq)

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

tokenize :: String -> a -> [Token a]
tokenize [] _ = []
tokenize ('\n':rest) a = Newline a : tokenize rest a
tokenize s@(c:_) a | isWs c = Whitespace ws a : tokenize rest a
    where
      (ws, rest) = break (not . isWs) s
tokenize s a = Token t a : tokenize rest a
    where
      (t, rest) = break (\c -> isWs c || c == '\n') s

serialize :: [Token a] -> String
serialize [] = []
serialize (Newline _:rest) = "\n" ++ serialize rest
serialize (Whitespace s _:rest) = s ++ serialize rest
serialize (Token s _:rest) = s ++ serialize rest

withAnnotation :: Token a -> a -> Token a
withAnnotation (Newline _) b = Newline b
withAnnotation (Whitespace s _) b = Whitespace s b
withAnnotation (Token s _) b = Token s b

isNewline :: Token a -> Bool
isNewline (Newline _) = True
isNewline _ = False

isWhitespace :: Token a -> Bool
isWhitespace (Whitespace _ _) = True
isWhitespace _ = False

splitLines :: (Eq a) => [Token a] -> [[Token a]]
splitLines ts = splitWith ts isNewline

-- |Truncate a token stream at a given column width.
trunc :: (Eq a) => a -> [Token a] -> Int -> [Token a]
trunc def ts width = concat $ intersperse [Newline def] newLines
    where
      newLines = map (\l -> truncLine width l) $ splitLines ts

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
