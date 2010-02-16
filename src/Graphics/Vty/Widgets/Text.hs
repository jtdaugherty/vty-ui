module Graphics.Vty.Widgets.Text
    ( Formatter
    , Text(defaultAttr, tokens)
    , wrap
    , simpleText
    , prepareText
    , textWidget
    , highlight
    )
where

import Data.List
    ( intersperse
    )
import Data.Maybe
    ( isJust
    )
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
import Text.Regex.PCRE.Light.Char8
    ( Regex
    , match
    , exec_anchored
    )

type Formatter = DisplayRegion -> Text -> Text
data Text = Text { defaultAttr :: Attr
                 , tokens :: [Token Attr]
                 }

prepareText :: Attr -> String -> Text
prepareText att s = Text { defaultAttr = att
                         , tokens = tokenize s att
                         }

simpleText :: Attr -> String -> Widget
simpleText a s = textWidget [] $ prepareText a s

wrap :: Formatter
wrap sz t = t { tokens = newTokens }
    where
      newTokens = concat $ intersperse [Newline $ defaultAttr t] $
                  map (concat .
                       intersperse [Newline $ defaultAttr t] .
                       wrapLine (defaultAttr t) (fromEnum $ region_width sz))
                  (splitLines $ tokens t)

-- |A highlight formatter takes a regular expression used to scan the
-- text and an attribute to assign to matches.  Highlighters only scan
-- non-whitespace tokens in the text stream.
highlight :: Regex -> Attr -> Formatter
highlight regex attr =
    \_ t -> t { tokens = map (annotate (matchesRegex regex) attr) $ tokens t }

annotate :: (Token a -> Bool) -> a -> Token a -> Token a
annotate f ann t = if f t then t `withAnnotation` ann else t

matchesRegex :: Regex -> Token a -> Bool
matchesRegex r (Token s _) = isJust $ match r s [exec_anchored]
matchesRegex _ _ = False

textWidget :: [Formatter] -> Text -> Widget
textWidget formatters t = Widget {
                            growHorizontal = False
                          , growVertical = False
                          , primaryAttribute = defaultAttr t
                          , withAttribute =
                              \att -> textWidget formatters $ newText att
                          , render = renderText t formatters
                          }
    where
      newText att = t { tokens = map (\c -> withAnnotation c att) $ tokens t }

applyFormatters :: [Formatter] -> DisplayRegion -> Text -> Text
applyFormatters [] _ t = t
applyFormatters (f:fs) sz t = applyFormatters fs sz $ f sz t

renderText :: Text -> [Formatter] -> DisplayRegion -> Render
renderText t formatters sz =
    if region_height sz == 0
    then renderImg nullImg
         else if null ls
              then renderImg nullImg
              else renderMany Vertical $ take (fromEnum $ region_height sz) lineImgs
    where
      -- Truncate the tokens at the specified column and split them up
      -- into lines
      newText = applyFormatters formatters sz t
      ls = splitLines (trunc (defaultAttr newText) (tokens newText)
                       (fromEnum $ region_width sz))
      lineImgs = map (renderImg . mkLineImg) ls
      mkLineImg line = if null line
                       then string (defaultAttr newText) " "
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""

      mkTokenImg (Newline _) = error "mkTokenImg should never be called on a Newline"
      mkTokenImg (Token s a) = string a s
      mkTokenImg (Whitespace s a) = string a s
