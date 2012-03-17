{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
-- |This module provides functionality for rendering 'String's as
-- 'Widget's, including functionality to make structural and/or visual
-- changes at rendering time.  To get started, turn your ordinary
-- 'String' into a 'Widget' with 'plainText'; for more control, use
-- 'textWidget'.
module Graphics.Vty.Widgets.Text
    ( FormattedText
    -- *Constructing Text Widgets
    , plainText
    , plainTextWithAttrs
    , textWidget
    -- *Setting Widget Contents
    , setText
    , setTextWithAttrs
    , setTextFormatter
    , setTextAppearFocused
    -- *Formatting
    , Formatter(Formatter)
    , applyFormatter
    , getTextFormatter
    , nullFormatter
    , wrap
    )
where

import Data.Monoid
import Data.Word
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Text.Trans.Tokenize
import Graphics.Vty.Widgets.Util

-- |A formatter makes changes to text at rendering time.  Some
-- formatting use cases involve knowing the size of the rendering
-- area, which is not known until render time (e.g., text wrapping).
-- Thus, a formatter takes a 'DisplayRegion' which indicates the size
-- of screen area available for formatting.
newtype Formatter = Formatter (DisplayRegion -> TextStream Attr -> IO (TextStream Attr))

instance Monoid Formatter where
    mempty = nullFormatter
    mappend (Formatter f1) (Formatter f2) =
        Formatter (\sz t -> f1 sz t >>= f2 sz)

applyFormatter :: Formatter -> DisplayRegion -> TextStream Attr -> IO (TextStream Attr)
applyFormatter (Formatter f) sz t = f sz t

-- |The null formatter which has no effect on text streams.
nullFormatter :: Formatter
nullFormatter = Formatter (\_ t -> return t)

-- |The type of formatted text widget state.  Stores the text itself
-- and the formatter used to apply attributes to the text.
data FormattedText =
    FormattedText { text :: TextStream Attr
                  , formatter :: !Formatter
                  , useFocusAttribute :: !Bool
                  }

instance Show FormattedText where
    show (FormattedText t _ f) = concat [ "FormattedText { "
                                        , "text = ", show t
                                        , ", formatter = ..."
                                        , ", useFocusAttribute = " ++ show f
                                        , " }"
                                        ]

-- |Construct a Widget directly from a String.  This is recommended if
-- you don't need to use a 'Formatter'.
plainText :: String -> IO (Widget FormattedText)
plainText = textWidget nullFormatter

-- |Construct a Widget directly from a list of strings and their
-- attributes.
plainTextWithAttrs :: [(String, Attr)] -> IO (Widget FormattedText)
plainTextWithAttrs pairs = do
  w <- textWidget nullFormatter ""
  setTextWithAttrs w pairs
  return w

-- |A formatter for wrapping text into the available space.  This
-- formatter will insert line breaks where appropriate so if you want
-- to use other structure-sensitive formatters, run this formatter
-- last.
wrap :: Formatter
wrap =
    Formatter $ \sz ts -> do
      let width = fromEnum $ region_width sz
      return $ wrapStream width ts

-- |Construct a text widget formatted with the specified formatters
-- and initial content.  The formatters will be applied in the order
-- given here (and, depending on the formatter, order might matter).
textWidget :: Formatter -> String -> IO (Widget FormattedText)
textWidget format s = do
  let initSt = FormattedText { text = TS []
                             , formatter = format
                             , useFocusAttribute = False
                             }

  wRef <- newWidget initSt $ \w ->
      w { getCursorPosition_ = const $ return Nothing
        , render_ =
            \this size ctx -> do
              ft <- getState this
              f <- focused <~ this
              appearFocused <- useFocusAttribute <~~ this
              renderText (text ft) (f && appearFocused) (formatter ft) size ctx
        }
  setText wRef s
  return wRef

-- |Set the formatter for the text.
setTextFormatter :: Widget FormattedText -> Formatter -> IO ()
setTextFormatter wRef f = updateWidgetState wRef $ \st ->
                          st { formatter = f }

-- |Get the formatter for the text.
getTextFormatter :: Widget FormattedText -> IO Formatter
getTextFormatter = (formatter <~~)

-- |Set whether a text widget can appear focused by using the
-- context-specific focus attribute when the widget has the focus.
-- This setting defaults to False; some widgets which embed text
-- widgets may need to turn this on.
setTextAppearFocused :: Widget FormattedText -> Bool -> IO ()
setTextAppearFocused wRef val = updateWidgetState wRef $ \st ->
                                st { useFocusAttribute = val }

-- |Set the text value of a 'FormattedText' widget.  The specified
-- string will be 'tokenize'd.
setText :: Widget FormattedText -> String -> IO ()
setText wRef s = setTextWithAttrs wRef [(s, def_attr)]

-- |Set the text value of a 'FormattedText' widget directly, in case
-- you have done formatting elsewhere and already have text with
-- attributes.  The specified strings will each be 'tokenize'd, and
-- tokens resulting from each 'tokenize' operation will be given the
-- specified attribute in the tuple.
setTextWithAttrs :: Widget FormattedText -> [(String, Attr)] -> IO ()
setTextWithAttrs wRef pairs = do
  let streams = map (\(s, a) -> tokenize s a) pairs
      ts = concat $ map streamEntities streams

  updateWidgetState wRef $ \st ->
      st { text = TS ts }

-- |Low-level text-rendering routine.
renderText :: TextStream Attr
           -> Bool
           -> Formatter
           -> DisplayRegion
           -> RenderContext
           -> IO Image
renderText t foc (Formatter format) sz ctx = do
  TS newText <- format sz t

  -- Truncate the tokens at the specified column and split them up
  -- into lines
  let attr' = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                         , normalAttr ctx
                         ]
      finalAttr tok = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                                 , tokenAttr tok
                                 , normalAttr ctx
                                 ]

      lineImgs = map mkLineImg ls
      ls = map truncLine $ map (map entityToken) $ findLines newText
      truncLine = truncateLine (fromEnum $ region_width sz)
      mkLineImg line = if null line
                       then char_fill attr' ' ' (region_width sz) (1::Word)
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""

      mkTokenImg :: Token Attr -> Image
      mkTokenImg tok = string (finalAttr tok) (tokenStr tok)

  return $ if region_height sz == 0
           then nullImg
           else if null ls || all null ls
                then nullImg
                else vert_cat $ take (fromEnum $ region_height sz) lineImgs
