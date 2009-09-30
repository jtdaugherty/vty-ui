module Graphics.Vty.Widgets.List
    ( List
    , mkList
    , scrollDown
    , scrollUp
    , getSelected
    )
where

import Graphics.Vty ( Attr, (<->) )
import Graphics.Vty.Widgets.Base
    ( Widget(..)
    , text
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

-- note that !! here will always succeed because selectedIndex will
-- never be out of bounds and the list will always be non-empty.
getSelected :: List -> String
getSelected list = (listItems list) !! (selectedIndex list)

scrollDown :: List -> List
scrollDown list
    -- If the list is already at the last position, do nothing.
    | selectedIndex list == length (listItems list) - 1 = list
    -- If the list requires scrolling the visible area, scroll it.
    | selectedIndex list == scrollTopIndex list + scrollWindowSize list - 1 =
        list { selectedIndex = selectedIndex list + 1
             , scrollTopIndex = scrollTopIndex list + 1
             }
    -- Otherwise, just increment the selectedIndex.
    | otherwise = list { selectedIndex = selectedIndex list + 1 }

scrollUp :: List -> List
scrollUp list
    -- If the list is already at the first position, do nothing.
    | selectedIndex list == 0 = list
    -- If the list requires scrolling the visible area, scroll it.
    | selectedIndex list == scrollTopIndex list =
        list { selectedIndex = selectedIndex list - 1
             , scrollTopIndex = scrollTopIndex list - 1
             }
    -- Otherwise, just decrement the selectedIndex.
    | otherwise = list { selectedIndex = selectedIndex list - 1 }

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

    render s w =
        foldl (<->) (head widgets) (tail widgets)
            where
              widgets = map (render s . mkWidget) (getVisibleItems w)
              mkWidget (str, selected) = let att = if selected
                                                   then selectedAttr
                                                   else normalAttr
                                         in text (att w) str