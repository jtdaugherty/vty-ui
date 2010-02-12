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

type Formatter = Text -> Text
data Text = Text { defaultAttr :: Attr
                 , tokens :: [Token Attr]
                 }

prepareText :: Attr -> String -> Text
prepareText att s = Text { defaultAttr = att
                         , tokens = tokenize s att
                         }

simpleText :: Attr -> String -> Widget
simpleText a s = textWidget $ prepareText a s

textWidget :: Text -> Widget
textWidget t = Widget {
                 growHorizontal = False
               , growVertical = False
               , primaryAttribute = defaultAttr t
               , withAttribute = textWidget . newText
               , render = renderText t
               }
    where
      newText att = t { tokens = map (\c -> withAnnotation c att) $ tokens t }

wrap :: Int -> Formatter
wrap width t = t { tokens = newTokens }
    where
      newTokens = concatMap (wrapLine (defaultAttr t) width)
                  (splitLines $ tokens t)

wrapWidget :: Text -> Widget
wrapWidget t = Widget {
                 growHorizontal = False
               , growVertical = False
               , primaryAttribute = defaultAttr t
               , withAttribute = wrapWidget . newText
               , render = \sz -> renderText (wrap (fromEnum $ region_width sz) t) sz
               }
    where
      newText att = t { tokens = map (`withAnnotation` att) $ tokens t }

-- XXX Still has a bug for completely blank lines (they get collapsed)
renderText :: Text -> DisplayRegion -> Render
renderText t sz =
    if null ls
    then renderImg nullImg
    else if region_height sz == 0
         then renderImg nullImg
         else renderMany Vertical $ take (fromEnum $ region_height sz) lineImgs
    where
      -- Truncate the tokens at the specified column and split them up
      -- into lines
      ls = splitLines (trunc (defaultAttr t) (tokens t) (fromEnum $ region_width sz))
      lineImgs = map (renderImg . mkLineImg) ls
      mkLineImg line = if null line
                       then string def_attr ""
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""

      mkTokenImg (Newline _) = error "mkTokenImg should never be called on a Newline"
      mkTokenImg (Token s a) = string a s
      mkTokenImg (Whitespace s a) = string a s
