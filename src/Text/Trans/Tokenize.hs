module Text.Trans.Tokenize
    ( Token(..)
    , tokenize
    , serialize
    , withAnnotation
    , trunc
    , splitWith
    , isNewline
    , splitLines
    , wrapLine
    )
where

import Data.List ( inits, intersperse )

data Token a = Newline a
             | Whitespace String a
             | Token String a
               deriving (Show, Eq)

splitWith :: (Eq a) => [a] -> (a -> Bool) -> [[a]]
splitWith [] _ = [[]]
splitWith es f = if null rest
                 then [first]
                 else first : splitWith (tail rest) f
    where
      (first, rest) = break f es

wsChars :: [Char]
wsChars = [' ', '\t']

isWhitespace :: Char -> Bool
isWhitespace = (`elem` wsChars)

tokenize :: String -> a -> [Token a]
tokenize [] _ = []
tokenize ('\n':rest) a = Newline a : tokenize rest a
tokenize s@(c:_) a | isWhitespace c = Whitespace ws a : tokenize rest a
    where
      (ws, rest) = break (not . isWhitespace) s
tokenize s a = Token t a : tokenize rest a
    where
      (t, rest) = break (\c -> isWhitespace c || c == '\n') s

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

wrapLine :: (Eq a) => a -> Int -> [Token a] -> [Token a]
wrapLine _ _ [] = []
wrapLine def width ts = take (length $ head passing) ts ++ [Newline def] ++
                        if null passing
                        then []
                        else wrapLine def width $ drop (length $ head passing) ts
    where
      lengths = map len ts
      cases = reverse $ inits lengths
      passing = dropWhile (\c -> sum c > width) cases
