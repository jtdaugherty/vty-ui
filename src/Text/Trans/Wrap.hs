-- |This module provides text-wrapping functionality.
module Text.Trans.Wrap
    ( wrap
    )
where

import Data.List ( intercalate )

-- |Given a position @p@ and string @s@, find the greatest string
-- index @i@ such that @i <= p@ and @s !! i@ is whitespace, or zero if
-- the string contains no whitespace.
findBreak :: Int -> String -> Int
findBreak 0 _ = 0
findBreak pos str = if str !! pos `elem` " \t"
                    then pos
                    else findBreak (pos - 1) str

-- |Given a column and string not containing newlines, break the
-- string into lines each having @length <= cols@.
wrapLine :: Int -> String -> [String]
wrapLine cols str
    | length str <= cols = [str]
    | otherwise = let breakpoint = findBreak cols str
                      first = take breakpoint str
                      rest = drop (breakpoint + 1) str
                  in if breakpoint /= 0
                     then first:(wrapLine cols rest)
                     else [str]

-- |Wraps the specified string (possibly containing newlines) at the
-- specified column and returns a new string with newlines inserted
-- where appropriate.
wrap :: Int -> String -> String
wrap cols s = intercalate "\n" $ concat $ map (wrapLine cols) $ lines s