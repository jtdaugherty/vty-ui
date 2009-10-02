-- |This module provides a widget which automatically wraps text in
-- the available space.  To create a 'WrappedText', see 'wrappedText'.
module Graphics.Vty.Widgets.WrappedText
    ( WrappedText
    , wrappedText
    )
where

import Graphics.Vty
    ( (<->)
    , Attr
    , region_width
    )
import Graphics.Vty.Widgets.Base
    ( Widget(..)
    , GrowthPolicy(Static)
    , text
    )

-- |A text widget which automatically wraps its contents to fit in the
-- available space.
data WrappedText = WrappedText Attr String

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

-- |Create a 'WrappedText' widget from the specified attribute and
-- text.
wrappedText :: Attr -> String -> WrappedText
wrappedText = WrappedText

instance Widget WrappedText where
    growthPolicy _ = Static

    primaryAttribute (WrappedText att _) = att

    render s (WrappedText attr str) =
        let widgets = map (render s . text attr) $ lines wrapped
            wrapped = wrap (fromEnum $ region_width s) str
        in foldl (<->) (head widgets) (tail widgets)
