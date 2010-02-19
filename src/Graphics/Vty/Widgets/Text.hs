-- |This module provides functionality for rendering 'String's as
-- 'Widget's, including functionality to make structural and/or visual
-- changes at rendering time.  To get started, turn your ordinary
-- 'String' into a 'Widget' with 'simpleText'; if you want access to
-- the 'Text' for formatting purposes, use 'prepareText' followed by
-- 'textWidget'.
module Graphics.Vty.Widgets.Text
    ( Text(defaultAttr, tokens)
    -- *Text Preparation
    , prepareText
    -- *Constructing Widgets
    , simpleText
    , textWidget
    -- *Formatting
    , (&.&)
    , Formatter
    , nullFormatter
    , highlight
    , wrap
    )
where

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
    , truncLine
    , wrapLine
    )
import Text.Regex.PCRE.Light.Char8
    ( Regex
    , match
    , exec_anchored
    )

-- |A formatter makes changes to text at rendering time.
--
-- It'd be nice if formatters were just @:: 'Text' -> 'Text'@, but
-- some formatting use cases involve knowing the size of the rendering
-- area, which is not known until render time (e.g., text wrapping).
-- Thus, a formatter takes a 'DisplayRegion' and runs at render time.
type Formatter = DisplayRegion -> Text -> Text

-- |Formatter composition: @a &.& b@ applies @a@ followed by @b@.
(&.&) :: Formatter -> Formatter -> Formatter
f1 &.& f2 = \sz -> f2 sz . f1 sz

-- |No-op formatter.
nullFormatter :: Formatter
nullFormatter = const id

-- |Text represents a String that can be manipulated with 'Formatter's
-- at rendering time.
data Text = Text { defaultAttr :: Attr
                 -- ^The default attribute for all tokens in this text
                 -- object.
                 , tokens :: [[Token Attr]]
                 -- ^The tokens of the underlying text stream.
                 }

-- |Tokenize a string and prepare it for rendering.
prepareText :: Attr -> String -> Text
prepareText att s = Text { defaultAttr = att
                         , tokens = tokenize s att
                         }

-- |Construct a Widget directly from an attribute and a String.  This
-- is recommended if you don't have any special formatting
-- requirements.
simpleText :: Attr -> String -> Widget
simpleText a s = textWidget nullFormatter $ prepareText a s

-- |A formatter for wrapping text into the available space (known at
-- rendering time).  This formatter will insert line breaks where
-- appropriate, so if you want to use other structure-sensitive
-- formatters, run this formatter last.
wrap :: Formatter
wrap sz t = t { tokens = newTokens }
    where
      newTokens = concatMap (wrapLine attr width) $ tokens t
      attr = defaultAttr t
      width = fromEnum $ region_width sz

-- |A highlight formatter takes a regular expression used to scan the
-- text and an attribute to assign to matches.  Highlighters only scan
-- non-whitespace tokens in the text stream.
highlight :: Regex -> Attr -> Formatter
highlight regex attr =
    \_ t -> t { tokens = map (map (annotate (matchesRegex regex) attr)) $ tokens t }

-- |Possibly annotate a token with the specified annotation value if
-- the predicate matches; otherwise, return the input token unchanged.
annotate :: (Token a -> Bool) -> a -> Token a -> Token a
annotate f ann t = if f t then t `withAnnotation` ann else t

-- |Does the specified regex match the token's string value?
matchesRegex :: Regex -> Token a -> Bool
matchesRegex r (Token s _) = isJust $ match r s [exec_anchored]
matchesRegex _ _ = False

-- |Construct a text widget formatted with the specified formatters.
-- the formatters will be applied in the order given here, so be aware
-- of how the formatters will modify the text (and affect each other).
textWidget :: Formatter -> Text -> Widget
textWidget formatter t = Widget {
                           growHorizontal = False
                         , growVertical = False
                         , primaryAttribute = defaultAttr t
                         , withAttribute =
                             \att -> textWidget formatter $ newText att
                         , render = renderText t formatter
                         }
    where
      newText att = t { tokens = map (map (`withAnnotation` att)) $ tokens t }

-- |Low-level text-rendering routine.
renderText :: Text -> Formatter -> DisplayRegion -> Render
renderText t formatter sz =
    if region_height sz == 0
    then renderImg nullImg
         else if null ls || all null ls
              then renderImg nullImg
              else renderMany Vertical $ take (fromEnum $ region_height sz) lineImgs
    where
      -- Truncate the tokens at the specified column and split them up
      -- into lines
      lineImgs = map (renderImg . mkLineImg) ls
      ls = map truncateLine $ tokens newText
      truncateLine = truncLine (fromEnum $ region_width sz)
      newText = formatter sz t
      mkLineImg line = if null line
                       then string (defaultAttr newText) " "
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""

      mkTokenImg (Token s a) = string a s
      mkTokenImg (Whitespace s a) = string a s
