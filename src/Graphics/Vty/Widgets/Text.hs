module Graphics.Vty.Widgets.Text
    ( Formatter
    , wrap
    , simpleText
    , prepareText
    , textWidget
    , wrapWidget
    )
where

import Graphics.Vty
    ( Attr
    , DisplayRegion
    , string
    , def_attr
    , horiz_cat
    , region_width
    , region_height
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Render
    , Orientation(Vertical)
    , renderMany
    , renderImg
    )
import Text.Trans.Tokenize
    ( Token(..)
    , tokenize
    , withAnnotation
    , trunc
    , splitLines
    , wrapLine
    )

type Formatter = [Token Attr] -> [Token Attr]

prepareText :: Attr -> String -> [Token Attr]
prepareText = flip tokenize

simpleText :: Attr -> String -> Widget
simpleText a s = textWidget $ prepareText a s

textWidget :: [Token Attr] -> Widget
textWidget ts = Widget {
                  growHorizontal = False
                , growVertical = False
                , primaryAttribute = def_attr
                , withAttribute = \a -> textWidget $ map (\c -> withAnnotation c a) ts
                , render = renderText ts
                }

wrap :: Int -> Formatter
wrap width ts = concatMap (wrapLine def_attr width) (splitLines ts)

wrapWidget :: [Token Attr] -> Widget
wrapWidget ts = Widget {
                  growHorizontal = False
                , growVertical = False
                , primaryAttribute = def_attr
                , withAttribute = \a -> wrapWidget $ map (`withAnnotation` a) ts
                , render = \sz -> renderText (wrap (fromEnum $ region_width sz) ts) sz
                }

-- XXX Still has a bug for completely blank lines (they get collapsed)
renderText :: [Token Attr] -> DisplayRegion -> Render
renderText content sz =
    if null ls
    then renderImg nullImg
    else if region_height sz == 0
         then renderImg nullImg
         else renderMany Vertical $ take (fromEnum $ region_height sz) lineImgs
    where
      -- Truncate the tokens at the specified column and split them up
      -- into lines
      ls = splitLines (trunc def_attr content (fromEnum $ region_width sz))
      lineImgs = map (renderImg . mkLineImg) ls
      mkLineImg line = if null line
                       then string def_attr ""
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""

      mkTokenImg (Newline _) = error "mkTokenImg should never be called on a Newline"
      mkTokenImg (Token s a) = string a s
      mkTokenImg (Whitespace s a) = string a s
