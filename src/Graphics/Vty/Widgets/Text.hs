{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |This module provides functionality for rendering 'String's as
-- 'Widget's, including functionality to make structural and/or visual
-- changes at rendering time.  To get started, turn your ordinary
-- 'String' into a 'Widget' with 'plainText'; if you want access to
-- the 'Text' for formatting purposes, use 'textWidget'.
module Graphics.Vty.Widgets.Text
    ( Text
    , FormattedText
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

    , findLines
    )
where

import qualified Data.Array as A
import Data.List ( findIndex )
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
type Formatter = DisplayRegion -> Text -> IO Text

-- |Formatter composition: @a &.& b@ applies @a@ followed by @b@.
(&.&) :: Formatter -> Formatter -> Formatter
f1 &.& f2 = \sz t -> f1 sz t >>= f2 sz

nullFormatter :: Formatter
nullFormatter = \_ t -> return t

-- |'Text' represents a 'String' that can be manipulated with
-- 'Formatter's at rendering time.
type Text = [(String, Attr)]

data FormattedText =
    FormattedText { text :: Text
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
plainText s = textWidget nullFormatter [(s, def_attr)]

-- |A formatter for wrapping text into the available space.  This
-- formatter will insert line breaks where appropriate so if you want
-- to use other structure-sensitive formatters, run this formatter
-- last.
wrap :: Formatter
wrap sz ts = return newTokens
    where
      doWrapping l = if null l then [[]] else wrapLine width l
      newTokens = concat $ concat $ map doWrapping $ findLines ts
      width = fromEnum $ region_width sz

stripNewlines :: [[(String, a)]] -> [[(String, a)]]
stripNewlines ls = map stripNewlines' ls
    where
      stripNewlines' = map stripNewline
      stripNewline (t, v) = (filter (/= '\n') t, v)

findLines :: [(String, a)] -> [[(String, a)]]
findLines ts = findLines' ts [[]]

findLines':: [(String, a)] -> [[(String, a)]] -> [[(String, a)]]
findLines' [] ls = ls
findLines' (t:rest) ls =
    case '\n' `elem` (fst t) of
      True -> let Just newlinePos = findIndex (== '\n') $ fst t
                  rest' = if newlinePos < ((length $ fst t) - 1)
                          then (drop (newlinePos + 1) (fst t), snd t) : rest
                          else rest
              in findLines' rest' $
                 concat [ init ls
                        -- If the newline comes at the end of this
                        -- token, just put the token in the list
                        -- rather than splitting it up.
                        , if newlinePos == ((length $ fst t) - 1)
                          then [last ls ++ [t]]
                          else [last ls ++ [(take (newlinePos + 1) (fst t), snd t)]]
                        , [[]]
                        ]
      False -> findLines' rest $ init ls ++ [last ls ++ [t]]

-- |A highlight formatter takes a regular expression used to scan the
-- text and an attribute to assign to matches.  Highlighters only scan
-- non-whitespace tokens in the text stream.
highlight :: Regex -> Attr -> Formatter
highlight regex attr =
    \_ ts -> return $ concat $ map highlightSplit ts
        where
          -- Tries to match all occurrences of the regex in this
          -- token.  Any matches will cause the token to be split up
          -- into a list of tokens where substrings which did not
          -- match will be in their own unformatted tokens and
          -- substrings which did match will be assigned the specified
          -- attribute.  This will NOT change the attribute of a token
          -- which already has one!
          highlightSplit (str, old_attr) =
              if old_attr /= def_attr
              then [(str, old_attr)]
              else highlightSplit' (str, old_attr)

          highlightSplit' (str, old_attr) =
              if null matches
              then [(str, old_attr)]
              else concat [ if first_match_start > 0
                            then [(take first_match_start str, def_attr)]
                            else []
                          , map (\(start, len, hl) ->
                                     ((take len $ drop start str), if hl then attr else old_attr))
                            (intervals matches)
                          -- ^ For each match, compute the string
                          -- between the match's start and end
                          -- positions and return that with the
                          -- desired attribute.
                          , if last_match_end < (length str - 1)
                            then [(drop last_match_end str, def_attr)]
                            else []
                          -- ^For the string indices prior to the
                          -- first match (and after the last match),
                          -- return an unformatted token.
                          ]
                  where
                    matches = map ((!! 0) . A.elems) $ matchAll regex str

                    intervals [] = []
                    intervals [(s1, l1)] = [(s1, l1, True)]
                    intervals ((s1, l1):r@(s2, _):rest) = [ (s1, l1, True)
                                                          , (s1 + l1, s2 - (s1 + l1), False)
                                                          ] ++ (intervals $ [r] ++ rest)

                    first_match = matches !! 0
                    last_match = matches !! (length matches - 1)

                    first_match_start = fst first_match
                    last_match_end = fst last_match + snd last_match

-- |Construct a text widget formatted with the specified formatters.
-- the formatters will be applied in the order given here, so be aware
-- of how the formatters will modify the text (and affect each other).
textWidget :: Formatter -> [(String, Attr)] -> IO (Widget FormattedText)
textWidget format ts = do
  wRef <- newWidget $ \w ->
      w { state = FormattedText { text = ts
                                , formatter = format
                                }
        , getCursorPosition_ = const $ return Nothing
        , render_ =
            \this size ctx -> do
              ft <- getState this
              f <- focused <~ this
              renderText (text ft) f (formatter ft) size ctx
        }
  return wRef

-- |Set the text value of a 'FormattedText' widget.
setText :: Widget FormattedText -> String -> IO ()
setText wRef s = do
  updateWidgetState wRef $ \st ->
      st { text = [(s, def_attr)] }

-- |Set the text value of a 'FormattedText' widget directly, in case
-- you have done formatting elsewhere and already have text with
-- attributes.
setTextWithAttrs :: Widget FormattedText -> [(String, Attr)] -> IO ()
setTextWithAttrs wRef ts = do
  updateWidgetState wRef $ \st ->
      st { text = ts }

-- |Low-level text-rendering routine.
renderText :: Text -> Bool -> Formatter -> DisplayRegion -> RenderContext -> IO Image
renderText t foc format sz ctx = do
  newText <- format sz t

  -- Truncate the tokens at the specified column and split them up
  -- into lines
  let attr' = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                         , normalAttr ctx
                         ]
      tokenAttr attr = mergeAttrs [ if foc then focusAttr ctx else overrideAttr ctx
                                  , attr
                                  , normalAttr ctx
                                  ]

      lineImgs = map mkLineImg ls
      ls = map truncateLine $ stripNewlines $ findLines newText
      truncateLine = truncLine (fromEnum $ region_width sz)
      mkLineImg line = if null line || (length line == 1 && ((fst $ line !! 0) == ""))
                       then char_fill attr' ' ' (region_width sz) (1::Word)
                       else horiz_cat $ map mkTokenImg line
      nullImg = string def_attr ""
      mkTokenImg (str, attr) = string (tokenAttr attr) str

  -- if length newText > 1 then error $ (show newText ++ "\n\n" ++ (show $ findLines newText)) else return ()

  return $ if region_height sz == 0
           then nullImg
           else if null ls || all null ls
                then nullImg
                else vert_cat $ take (fromEnum $ region_height sz) lineImgs
