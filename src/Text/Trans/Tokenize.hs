{-# LANGUAGE CPP #-}
module Text.Trans.Tokenize
    ( TextStream(..)
    , TextStreamEntity(..)
    , Token(..)
    , tokenLen
    , entityToken
    , truncLine
    , wrapStream
    , tokenize
    , serialize
    , findLines
#ifdef TESTING
    , isWhitespace
    , partitions
#endif
    )
where

import Data.List
    ( inits
    )

data Token a = S { tokenStr :: String
                 , tokenAttr :: a
                 }
             | WS { tokenStr :: String
                  , tokenAttr :: a
                  }

data TextStreamEntity a = T (Token a)
                        | NL

data TextStream a = TS [TextStreamEntity a]

instance (Show a) => Show (TextStream a) where
    show (TS ts) = "TS " ++ show ts

instance (Show a) => Show (TextStreamEntity a) where
    show (T t) = "T " ++ show t
    show NL = "NL"

instance (Show a) => Show (Token a) where
    show (S s a) = "S " ++ show s ++ " " ++ show a
    show (WS s a) = "WS " ++ show s ++ " " ++ show a

instance (Eq a) => Eq (Token a) where
    a == b = (tokenStr a) == (tokenStr b) &&
             (tokenAttr a) == (tokenAttr b)

instance (Eq a) => Eq (TextStreamEntity a) where
    NL == NL = True
    T a == T b = a == b
    _ == _ = False

instance (Eq a) => Eq (TextStream a) where
    (TS as) == (TS bs) = as == bs

tokenLen :: Token a -> Int
tokenLen (S s _) = length s
tokenLen (WS s _) = length s

wsChars :: [Char]
wsChars = [' ', '\t']

isWs :: Char -> Bool
isWs = (`elem` wsChars)

isNL :: TextStreamEntity a -> Bool
isNL NL = True
isNL _ = False

entityToken :: TextStreamEntity a -> Token a
entityToken (T t) = t
entityToken _ = error "Cannot get token from non-token entity"

isWhitespace :: Token a -> Bool
isWhitespace (WS _ _) = True
isWhitespace _ = False

isWsEnt :: TextStreamEntity a -> Bool
isWsEnt (T (WS _ _)) = True
isWsEnt _ = False

serialize :: TextStream a -> String
serialize (TS es) = concat $ map serializeEntity es
    where
      serializeEntity NL = "\n"
      serializeEntity (T (WS s _)) = s
      serializeEntity (T (S s _)) = s

tokenize :: String -> a -> TextStream a
tokenize s def = TS $ findEntities s
    where
      findEntities [] = []
      findEntities str@(c:_) = nextEntity : findEntities (drop nextLen str)
          where
            (nextEntity, nextLen) = if isWs c
                                    then (T (WS nextWs def), length nextWs)
                                    else if c == '\n'
                                         then (NL, 1)
                                         else (T (S nextStr def), length nextStr)
            nextWs = takeWhile isWs str
            nextStr = takeWhile (\ch -> not $ ch `elem` ('\n':wsChars)) str

-- |Given a list of tokens, truncate the list so that its underlying
-- string representation does not exceed the specified column width.
truncLine :: Int -> [Token a] -> [Token a]
truncLine l _ | l < 0 = error $ "truncLine cannot truncate at length = " ++ show l
truncLine _ [] = []
truncLine width ts =
    -- If we are returning all tokens, we didn't have to do any
    -- truncation.  But if we *did* have to truncate, return exactly
    -- 'width' characters' worth of tokens by constructing a new final
    -- token with the same attribute data.
    --
    -- If there are no passing cases (i.e., remaining is null), just
    -- return 'width' characters of the first token.
    if null remaining
    then [first_tok { tokenStr = take width $ tokenStr first_tok }]
    else if length tokens == length ts
         then tokens
         else if null $ tokenStr lastToken
              then tokens
              else tokens ++ [lastToken]
    where
      lengths = map (length . tokenStr) ts
      cases = reverse $ inits lengths
      remaining = dropWhile ((> width) . sum) cases
      tokens = take (length $ head remaining) ts
      truncLength = sum $ head remaining

      first_tok = ts !! 0
      last_tok = ts !! (length tokens)
      lastToken = last_tok { tokenStr = take (width - truncLength) $ tokenStr last_tok }

wrapStream :: (Eq a) => Int -> TextStream a -> TextStream a
wrapStream width (TS stream) = TS $ reverse $ dropWhile (== NL) $ reverse $ wrapAll' 0 stream
    where
      wrapAll' :: Int -> [TextStreamEntity a] -> [TextStreamEntity a]
      wrapAll' _ [] = []
      wrapAll' _ (NL:rest) = NL : wrapAll' 0 rest
      wrapAll' accum (T t:rest) =
          if (length $ tokenStr t) + accum > width
          then if isWhitespace t
               then [NL] ++ wrapAll' 0 (dropWhile isWsEnt rest)
               else if accum == 0 && ((length $ tokenStr t) >= width)
                    then [T t, NL] ++ wrapAll' 0 (dropWhile isWsEnt rest)
                    else [NL, T t] ++ wrapAll' (length $ tokenStr t) rest
          else T t : wrapAll' (accum + (length $ tokenStr t)) rest

partitions :: (a -> Bool) -> [a] -> [[a]]
partitions _ [] = []
partitions f as = p : partitions f (drop (length p + 1) as)
    where
      p = takeWhile f as

findLines :: [TextStreamEntity a] -> [[TextStreamEntity a]]
findLines = partitions (not . isNL)
