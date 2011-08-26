{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |This module provides functionality for rendering 'String's as
-- 'Widget's, including functionality to make structural and/or visual
-- changes at rendering time.  To get started, turn your ordinary
-- 'String' into a 'Widget' with 'plainText'; if you want access to
-- the 'Text' for formatting purposes, use 'textWidget'.
module Graphics.Vty.Widgets.Text
    ( FormattedText
    , Formatter
    , setText
    , setTextWithAttrs
    -- *Constructing Widgets
    , plainText
    , textWidget
    -- *Formatting
    , (&.&)
    , highlight
    , nullFormatter
    , wrap
    )
where

import Data.Word
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Text.Trans.Tokenize
import Text.Regex.PCRE
import Graphics.Vty.Widgets.Util

-- |A formatter makes changes to text at rendering time.
--
-- It'd be nice if formatters were just @:: 'Text' -> 'Text'@, but
-- some formatting use cases involve knowing the size of the rendering
-- area, which is not known until render time (e.g., text wrapping).
-- Thus, a formatter takes a 'DisplayRegion' and runs at render time.
type Formatter = DisplayRegion -> TextStream Attr -> IO (TextStream Attr)

-- |Formatter composition: @a &.& b@ applies @a@ followed by @b@.
(&.&) :: Formatter -> Formatter -> Formatter
f1 &.& f2 = \sz t -> f1 sz t >>= f2 sz

nullFormatter :: Formatter
nullFormatter = \_ t -> return t

data FormattedText =
    FormattedText { text :: TextStream Attr
                  , formatter :: Formatter
                  }

instance Show FormattedText where
    show (FormattedText t _) = concat [ "FormattedText { "
                                      , "text = ", show t
                                      , ", formatter = ... }"
                                      ]

-- |Construct a Widget directly from a String.  This is recommended if
-- you don't need to use a 'Formatter'.
plainText :: String -> IO (Widget FormattedText)
plainText = textWidget nullFormatter

-- |A formatter for wrapping text into the available space.  This
-- formatter will insert line breaks where appropriate so if you want
-- to use other structure-sensitive formatters, run this formatter
-- last.
wrap :: Formatter
wrap sz ts = do
  let width = fromEnum $ region_width sz
  return $ wrapStream width ts

-- |A highlight formatter takes a regular expression used to scan the
-- text and an attribute to assign to matches.  Highlighters only scan
-- non-whitespace tokens in the text stream.
highlight :: Regex -> Attr -> Formatter
highlight regex attr =
    \_ (TS ts) -> return $ TS $ map highlightToken ts
        where
          highlightToken :: TextStreamEntity Attr -> TextStreamEntity Attr
          highlightToken NL = NL
          highlightToken (T t) =
              if tokenAttr t /= def_attr
              then T t
              else T (highlightToken' t)

          highlightToken' :: Token Attr -> Token Attr
          highlightToken' t =
              if null $ matchAll regex $ tokenStr t
              then t
              else t { tokenAttr = attr }

-- |Construct a text widget formatted with the specified formatters.
-- the formatters will be applied in the order given here, so be aware
-- of how the formatters will modify the text (and affect each other).
textWidget :: Formatter -> String -> IO (Widget FormattedText)
textWidget format s = do
  wRef <- newWidget $ \w ->
      w { state = FormattedText { text = TS []
                                , formatter = format
                                }
        , getCursorPosition_ = const $ return Nothing
        , render_ =
            \this size ctx -> do
              ft <- getState this
              f <- focused <~ this
              renderText (text ft) f (formatter ft) size ctx
        }
  setText wRef s
  return wRef

-- |Set the text value of a 'FormattedText' widget.
setText :: Widget FormattedText -> String -> IO ()
setText wRef s = setTextWithAttrs wRef [(s, def_attr)]

-- |Set the text value of a 'FormattedText' widget directly, in case
-- you have done formatting elsewhere and already have text with
-- attributes.
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
renderText t foc format sz ctx = do
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
