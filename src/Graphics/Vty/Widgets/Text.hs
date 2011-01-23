-- |This module provides functionality for rendering 'String's as
-- 'Widget's, including functionality to make structural and/or visual
-- changes at rendering time.  To get started, turn your ordinary
-- 'String' into a 'Widget' with 'simpleText'; if you want access to
-- the 'Text' for formatting purposes, use 'prepareText' followed by
-- 'textWidget'.
module Graphics.Vty.Widgets.Text
    ( Text(defaultAttr, tokens)
    , FormattedText
    , Formatter
    , setText
    -- *Text Preparation
    , prepareText
    , resetText
    -- *Constructing Widgets
    , simpleText
    , textWidget
    -- *Formatting
    , (&.&)
    , highlight
    , nullFormatter
    , wrap
    )
where

import Control.Monad.Trans
    ( MonadIO
    )
import Data.Maybe
    ( isJust
    , catMaybes
    )
import Graphics.Vty
    ( Attr
    , DisplayRegion
    , Image
    , string
    , def_attr
    , horiz_cat
    , vert_cat
    , region_width
    , region_height
    )
import Graphics.Vty.Widgets.Core
    ( WidgetImpl(..)
    , Widget
    , RenderContext(overrideAttr)
    , newWidget
    , updateWidget
    , updateWidgetState
    , getState
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

nullFormatter :: Formatter
nullFormatter = const id

-- |'Text' represents a 'String' that can be manipulated with
-- 'Formatter's at rendering time.
data Text = Text { defaultAttr :: Attr
                 -- ^The default attribute for all tokens in this text
                 -- object.
                 , tokens :: [[Token Attr]]
                 -- ^The tokens of the underlying text stream.
                 }
            deriving (Show)

data FormattedText =
    FormattedText { text :: Text
                  , formatter :: Formatter
                  }

instance Show FormattedText where
    show (FormattedText t _) = concat [ "FormattedText { "
                                      , "text = ", show t
                                      , ", formatter = ... }"
                                      ]

resetText :: Text -> Text
resetText t =
    t { tokens = map (map (`withAnnotation` (defaultAttr t))) $ tokens t }

-- |Prepare a string for rendering and assign it the specified default
-- attribute.
prepareText :: Attr -> String -> Text
prepareText att s = Text { defaultAttr = att
                         , tokens = tokenize s att
                         }

-- |Construct a Widget directly from an attribute and a String.  This
-- is recommended if you don't need to use a 'Formatter'.
simpleText :: (MonadIO m) => Attr -> String -> m (Widget FormattedText)
simpleText a s = textWidget nullFormatter $ prepareText a s

-- |A formatter for wrapping text into the available space.  This
-- formatter will insert line breaks where appropriate so if you want
-- to use other structure-sensitive formatters, run this formatter
-- last.
wrap :: Formatter
wrap sz t = t { tokens = newTokens }
    where
      doWrapping l = if null l then [[]] else wrapLine width l
      newTokens = concatMap doWrapping $ tokens t
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
matchesRegex r t = isJust $ match r (tokenString t) [exec_anchored]

-- |Construct a text widget formatted with the specified formatters.
-- the formatters will be applied in the order given here, so be aware
-- of how the formatters will modify the text (and affect each other).
textWidget :: (MonadIO m) => Formatter -> Text -> m (Widget FormattedText)
textWidget format t = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = FormattedText { text = t
                                , formatter = format
                                }
        , getGrowHorizontal = const $ return False
        , getGrowVertical = const $ return False
        , draw =
            \this size ctx -> do
              ft <- getState this
              return $ renderText (text ft) (formatter ft) size ctx
        }
  return wRef

setText :: (MonadIO m) => Widget FormattedText -> Attr -> String -> m ()
setText wRef attr s =
    updateWidgetState wRef $ \st -> st { text = prepareText attr s }

-- |Low-level text-rendering routine.
renderText :: Text -> Formatter -> DisplayRegion -> RenderContext -> Image
renderText t format sz ctx =
    if region_height sz == 0
    then nullImg
         else if null ls || all null ls
              then nullImg
              else vert_cat $ take (fromEnum $ region_height sz) lineImgs
    where
      -- Truncate the tokens at the specified column and split them up
      -- into lines
      attr' = head $ catMaybes [ overrideAttr ctx, Just $ defaultAttr newText ]
      tokenAttr tok = head $ catMaybes [ overrideAttr ctx, Just $ tokenAnnotation tok ]

      lineImgs = map mkLineImg ls
      ls = map truncateLine $ tokens newText
      truncateLine = truncLine (fromEnum $ region_width sz)
      newText = format sz t
      mkLineImg line = if null line
                       then string attr' " "
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""
      mkTokenImg tok = string (tokenAttr tok) (tokenString tok)
