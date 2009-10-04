module Text.Trans.Wrap
    ( wrap
    )
where

import Data.List ( intercalate )

findBreak :: Int -> String -> Int
findBreak 0 _ = 0
findBreak pos str = if str !! pos `elem` " \t"
                    then pos
                    else findBreak (pos - 1) str

wrapLine :: Int -> String -> [String]
wrapLine cols str
    | length str <= cols = [str]
    | otherwise = let breakpoint = findBreak cols str
                      first = take breakpoint str
                      rest = drop (breakpoint + 1) str
                  in if breakpoint /= 0
                     then first:(wrapLine cols rest)
                     else [str]

wrap :: Int -> String -> String
wrap cols s = intercalate "\n" $ concat $ map (wrapLine cols) $ lines s