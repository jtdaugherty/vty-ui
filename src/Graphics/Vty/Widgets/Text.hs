module Graphics.Vty.Widgets.Text
    ( Formatter
    , text
    , simpleText
    , prepareText
    )
where

import Graphics.Vty
    ( Attr
    , DisplayRegion
    , string
    , def_attr
    , horiz_cat
    , vert_cat
    , region_width
    , region_height
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Render
    , renderImg
    )
import Text.Trans.Tokenize
    ( Token(..)
    , tokenize
    , withAnnotation
    , trunc
    , splitWith
    , isNewline
    )

type Formatter = [Token Attr] -> [Token Attr]

prepareText :: Attr -> String -> [Token Attr]
prepareText = flip tokenize

simpleText :: Attr -> String -> Widget
simpleText a s = text $ prepareText a s

-- |A text widget consisting of a string rendered using an
-- attribute. See 'text'.
text :: [Token Attr] -> Widget
text content = Widget {
                 growHorizontal = False
               , growVertical = False
               , primaryAttribute = def_attr
               , withAttribute = \a -> text $ map (\c -> withAnnotation c a) content
               , render = renderText content
               }

-- XXX use sz to truncate...
renderText :: [Token Attr] -> DisplayRegion -> Render
renderText content sz =
    renderImg $ if null ls
                then nullImg
                else if region_height sz == 0
                     then nullImg
                     else vert_cat $
                          take (fromEnum $ region_height sz) lineImgs
    where
      -- Truncate the tokens at the specified column and split them up
      -- into lines
      ls = splitWith (trunc content (fromEnum $ region_width sz)) isNewline
      lineImgs = map mkLineImg ls
      mkLineImg line = horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""

      mkTokenImg (Newline a) = string a " "
      mkTokenImg (Token s a) = string a s
      mkTokenImg (Whitespace s a) = string a s
