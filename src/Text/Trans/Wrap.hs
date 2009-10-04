module Text.Trans.Wrap
    ( wrap
    )
where

nextLine :: Int -> String -> (String, Maybe String)
nextLine cols s
    | length s <= cols = (s, Nothing)
    | otherwise = (line, rest)
    where
      breakpoint = findBreak cols s
      (line, rest) = let (h, t) = splitAt breakpoint s
                     in if breakpoint == 0
                        then (s, Nothing)
                        else (h, Just $ drop 1 t)
      findBreak 0 _ = 0
      findBreak pos str = if str !! pos `elem` " \t"
                          then pos
                          else findBreak (pos - 1) str

wrap :: Int -> String -> String
wrap cols s = first ++ "\n" ++ rest
    where (first, mRest) = nextLine cols s
          rest = maybe "" (wrap cols) mRest