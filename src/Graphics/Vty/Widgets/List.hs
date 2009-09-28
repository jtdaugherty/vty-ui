module Graphics.Vty.Widgets.List
    ( List
    , mkList
    )
where

import Graphics.Vty ( Attr, (<->) )
import Graphics.Vty.Widgets.Base
    ( Widget(..)
    , Text(Text)
    , GrowthPolicy(Static)
    )

data List = List { normalAttr :: Attr
                 , selectedAttr :: Attr
                 , selectedIndex :: Int
                 , scrollTopIndex :: Int
                 , scrollWindowSize :: Int
                 , listItems :: [String]
                 }

mkList :: Attr -> Attr -> Int -> [String] -> List
mkList _ _ _ [] = error "Lists cannot be empty"
mkList normAttr selAttr swSize contents = List normAttr selAttr 0 0 swSize contents

getVisibleItems :: List -> [(String, Bool)]
getVisibleItems list =
    let start = scrollTopIndex list
        stop = scrollTopIndex list + scrollWindowSize list
        adjustedStop = (min stop $ length $ listItems list) - 1
    in [ (listItems list !! i, i == selectedIndex list)
             | i <- [start..adjustedStop] ]

instance Widget List where
    -- Statically sized, because we know how many items should be
    -- visible.
    growthPolicy _ = Static
    attr = normalAttr

    render s w =
        foldl (<->) (head widgets) (tail widgets)
            where
              widgets = map (render s . mkWidget) (getVisibleItems w)
              mkWidget (str, selected) = let att = if selected
                                                   then selectedAttr
                                                   else normalAttr
                                         in Text (att w) str