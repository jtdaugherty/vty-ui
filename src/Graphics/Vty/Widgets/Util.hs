module Graphics.Vty.Widgets.Util
    ( on
    , fgColor
    , bgColor
    , style
    , mergeAttr
    , mergeAttrs
    , withWidth
    , withHeight
    , plusWidth
    , plusHeight
    , remove
    , inject
    , repl
    )
where

import Data.Word
import Graphics.Vty

-- |Infix attribute constructor.  Use: foregroundColor `on`
-- backgroundColor.
on :: Color -> Color -> Attr
on a b = def_attr `with_back_color` b `with_fore_color` a

-- |Foreground-only attribute constructor.  Background color and style
-- are defaulted.
fgColor :: Color -> Attr
fgColor = (def_attr `with_fore_color`)

-- |Background-only attribute constructor.  Foreground color and style
-- are defaulted.
bgColor :: Color -> Attr
bgColor = (def_attr `with_back_color`)

-- |Style-only attribute constructor.  Colors are defaulted.
style :: Style -> Attr
style = (def_attr `with_style`)

-- Left-most attribute's fields take precedence.
-- |Merge two attributes.  Leftmost attribute takes precedence where
-- it specifies any of the foreground color, background color, or
-- style.  Note that the style precedence is total: all bits of the
-- style mask will take precedence if any are set.
mergeAttr :: Attr -> Attr -> Attr
mergeAttr a b =
    let b1 = case attr_style a of
               SetTo v -> b { attr_style = SetTo v }
               _ -> b
        b2 = case attr_fore_color a of
               SetTo v -> b1 `with_fore_color` v
               _ -> b1
        b3 = case attr_back_color a of
               SetTo v -> b2 `with_back_color` v
               _ -> b2
    in b3

-- |List fold version of 'mergeAttr'.
mergeAttrs :: [Attr] -> Attr
mergeAttrs attrs = foldr mergeAttr def_attr attrs

-- |Modify the width component of a 'DisplayRegion'.
withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

-- |Modify the height component of a 'DisplayRegion'.
withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h

-- |Modify the width component of a 'DisplayRegion'.
plusWidth :: DisplayRegion -> Word -> DisplayRegion
plusWidth (DisplayRegion w' h) w =
    if (fromEnum w' + fromEnum w < 0)
    then error $ "plusWidth: would overflow on " ++ (show w') ++ " + " ++ (show w)
    else DisplayRegion (w + w') h

-- |Modify the height component of a 'DisplayRegion'.
plusHeight :: DisplayRegion -> Word -> DisplayRegion
plusHeight (DisplayRegion w h') h =
    if (fromEnum h' + fromEnum h < 0)
    then error $ "plusHeight: would overflow on " ++ (show h') ++ " + " ++ (show h)
    else DisplayRegion w (h + h')

remove :: Int -> [a] -> [a]
remove pos as = (take pos as) ++ (drop (pos + 1) as)

inject :: Int -> a -> [a] -> [a]
inject pos a as = let (h, t) = splitAt pos as
                  in h ++ (a:t)

repl :: Int -> a -> [a] -> [a]
repl pos a as = inject pos a (remove pos as)