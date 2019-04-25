{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}
-- |This module provides a text-editing widget.  Edit widgets can
-- operate in single- and multi-line modes.
--
-- Edit widgets support the following special keystrokes:
--
-- * Arrow keys to navigate the text
--
-- * @Enter@ - Activate single-line edit widgets or insert new lines
--   into multi-line widgets
--
-- * @Home@ / @Control-a@ - Go to beginning of the current line
--
-- * @End@ / @Control-e@ - Go to end of the current line
--
-- * @Control-k@ - Remove text from the cursor to the end of the line,
--   or remove the line if it is empty
--
-- * @Del@ / @Control-d@ - delete the current character
--
-- * @Backspace@ - delete the previous character
--
-- Edit widgets may be configured with a line limit which limits the
-- number of lines of text the widget will store.  It does not provide
-- any limit control on the length of its lines, though.
--
-- Edit widgets support multi-column characters.  (For some
-- information, see <http://www.unicode.org/reports/tr11/>.)  When the
-- edit widget scrolling reaches a point where a wide character cannot
-- be drawn because it is bisected by the editing window's boundary,
-- it will be replaced with an indicator (\"$\") until the scrolling
-- window is moved enough to reveal the character.  This is done to
-- preserve the relative alignment of all of the rows in the widget in
-- the presence of characters of different widths.  Note that this is
-- a visual aid only and does not affect the underlying text content
-- of the widget.
module Graphics.Vty.Widgets.Edit
    ( Edit
    , editWidget
    , multiLineEditWidget
    , getEditText
    , getEditCurrentLine
    , setEditText
    , setEditRewriter
    , setCharFilter
    , getEditCursorPosition
    , setEditCursorPosition
    , setEditLineLimit
    , getEditLineLimit
    , setEditMaxLength
    , getEditMaxLength
    , applyEdit
    , onActivate
    , onChange
    , onCursorMove
#ifdef TESTING
    , doClipping
    , indicatorChar
#endif
    )
where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe (isJust)
import qualified Data.Text as T
import Graphics.Vty hiding (regionHeight, regionWidth)
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.TextClip
import qualified Graphics.Vty.Widgets.TextZipper as Z

data Edit = Edit { contents :: Z.TextZipper T.Text
                 , clipRect :: ClipRect
                 , activateHandlers :: Handlers (Widget Edit)
                 , changeHandlers :: Handlers T.Text
                 , cursorMoveHandlers :: Handlers (Int, Int)
                 , lineLimit :: Maybe Int
                 , maxLength :: Maybe Int
                 , rewriter :: Char -> Char
                 , charFilter :: Char -> Bool
                 }

instance Show Edit where
    show e = concat [ "Edit { "
                    , "contents = ", show $ contents e
                    , ", lineLimit = ", show $ lineLimit e
                    , ", clipRect = ", show $ clipRect e
                    , " }"
                    ]

editWidget' :: IO (Widget Edit)
editWidget' = do
  ahs <- newHandlers
  chs <- newHandlers
  cmhs <- newHandlers

  let initSt = Edit { contents = Z.textZipper []
                    , clipRect = ClipRect { clipLeft = 0
                                          , clipWidth = 0
                                          , clipTop = 0
                                          , clipHeight = 1
                                          }
                    , activateHandlers = ahs
                    , changeHandlers = chs
                    , cursorMoveHandlers = cmhs
                    , lineLimit = Nothing
                    , maxLength = Nothing
                    , rewriter = id
                    , charFilter = const True
                    }

  wRef <- newWidget initSt $ \w ->
      w { growHorizontal_ = const $ return True
        , growVertical_ =
            \this -> do
              case lineLimit this of
                Just v | v == 1 -> return False
                _ -> return True

        , getCursorPosition_ = internalGetCursorPosition
        , render_ = renderEditWidget
        , keyEventHandler = editKeyEvent
        }
  return wRef

internalGetCursorPosition :: Widget Edit -> IO (Maybe DisplayRegion)
internalGetCursorPosition this = do
  st <- getState this
  f <- focused <~ this
  pos <- getCurrentPosition this

  let (cursorRow, _) = Z.cursorPosition (contents st)
      Phys offset = physCursorCol st - clipLeft (clipRect st)
      newPos = pos
               `withWidth` (toEnum ((fromEnum $ regionWidth pos) + offset))
               `plusHeight` (toEnum (cursorRow - (fromEnum $ clipTop $ clipRect st)))

  return $ if f then Just newPos else Nothing

-- |Set the function which rewrites all characters at rendering time.  Defaults
-- to 'id'.  Does not affect text stored in the editor.
setEditRewriter :: Widget Edit -> (Char -> Char) -> IO ()
setEditRewriter w f =
    updateWidgetState w $ \st -> st { rewriter = f }

-- |Set the function which allows typed characters in the edit widget.
-- Defaults to 'const' 'True', allowing all characters.  For example, setting the
-- filter to ('elem' \"0123456789\") will only allow numbers in the edit widget.
setCharFilter :: Widget Edit -> (Char -> Bool) -> IO ()
setCharFilter w f =
    updateWidgetState w $ \st -> st { charFilter = f }

renderEditWidget :: Widget Edit -> DisplayRegion -> RenderContext -> IO Image
renderEditWidget this size ctx = do
  resizeEdit this ( Phys $ fromEnum $ regionHeight size
                  , Phys $ fromEnum $ regionWidth size )

  st <- getState this
  isFocused <- focused <~ this

  let nAttr = mergeAttrs [ overrideAttr ctx
                         , normalAttr ctx
                         ]
      attr = if isFocused then focusAttr ctx else nAttr

      clipped = doClipping (Z.getText $ contents st) (clipRect st)
      rewritten = ((rewriter st) <$>) <$> clipped

      totalAllowedLines = fromEnum $ regionHeight size
      numEmptyLines = lim - length rewritten
          where
            lim = case lineLimit st of
                    Just v -> min v totalAllowedLines
                    Nothing -> totalAllowedLines

      emptyLines = replicate numEmptyLines ""
      lineWidget s = let Phys physLineLength = sum $ chWidth <$> s
                     in string attr s <|>
                        charFill attr ' ' (regionWidth size - toEnum physLineLength) 1

  return $ vertCat $ lineWidget <$> (rewritten ++ emptyLines)

doClipping :: [T.Text] -> ClipRect -> [String]
doClipping ls rect =
    let sliced True = [indicatorChar]
        sliced False = ""
        truncatedLines = clip2d rect ls

    in [ sliced lslice ++ (T.unpack r) ++ sliced rslice
         | (r, lslice, rslice) <- truncatedLines ]

-- |Convert a logical column number (corresponding to a character) to
-- a physical column number (corresponding to a terminal cell).
toPhysical :: Int -> [Char] -> Phys
toPhysical col line = sum $ chWidth <$> take col line

indicatorChar :: Char
indicatorChar = '$'

-- |Construct a text widget for editing a single line of text.
-- Single-line edit widgets will send activation events when the user
-- presses @Enter@ (see 'onActivate').
editWidget :: IO (Widget Edit)
editWidget = do
  wRef <- editWidget'
  setNormalAttribute wRef $ style underline
  setFocusAttribute wRef $ style underline
  setEditLineLimit wRef $ Just 1
  return wRef

-- |Construct a text widget for editing multi-line documents.
-- Multi-line edit widgets never send activation events, since the
-- @Enter@ key inserts a new line at the cursor position.
multiLineEditWidget :: IO (Widget Edit)
multiLineEditWidget = do
  wRef <- editWidget'
  setEditLineLimit wRef Nothing
  return wRef

-- |Set the limit on the number of lines for the edit widget.
-- 'Nothing' indicates no limit, while 'Just' indicates a limit of the
-- specified number of lines.
setEditLineLimit :: Widget Edit -> Maybe Int -> IO ()
setEditLineLimit _ (Just v) | v <= 0 = return ()
setEditLineLimit w v = updateWidgetState w $ \st -> st { lineLimit = v }

-- |Get the current line limit, if any, for the edit widget.
getEditLineLimit :: Widget Edit -> IO (Maybe Int)
getEditLineLimit = (lineLimit <~~)

-- |Set the maximum length of the contents of an edit widget.  Applies to every
-- line of text in the editor.  'Nothing' indicates no limit, while 'Just'
-- indicates a limit of the specified number of characters.
setEditMaxLength :: Widget Edit -> Maybe Int -> IO ()
setEditMaxLength _ (Just v) | v <= 0 = return ()
setEditMaxLength w v = updateWidgetState w $ \st -> st { maxLength = v }

-- |Get the current maximum length, if any, for the edit widget.
getEditMaxLength :: Widget Edit -> IO (Maybe Int)
getEditMaxLength = (maxLength <~~)

resizeEdit :: Widget Edit -> (Phys, Phys) -> IO ()
resizeEdit e (newHeight, newWidth) = do
  updateWidgetState e $ \st ->
      let newRect = (clipRect st) { clipHeight = newHeight
                                  , clipWidth = newWidth
                                  }
          (cursorRow, _) = Z.cursorPosition $ contents st
          adjusted = updateRect (Phys cursorRow, physCursorCol st) newRect
      in st { clipRect = adjusted }

  updateWidgetState e $ \s ->
      let r = clipRect s
          (_, cursorColumn) = Z.cursorPosition $ contents s
          curLine = T.unpack $ Z.currentLine $ contents s
          (_, _, ri) = clip1d (clipLeft r) (clipWidth r) (T.pack curLine)
          newCharLen = if cursorColumn >= 0 && cursorColumn < length curLine
                       then chWidth $ curLine !! cursorColumn
                       else Phys 1

          newPhysCol = toPhysical cursorColumn curLine
          extra = if ri && newPhysCol >= ((clipLeft r) + (clipWidth r) - Phys 1)
                  then newCharLen - 1
                  else 0
          newLeft = clipLeft (clipRect s) + extra
      in s { clipRect = (clipRect s) { clipLeft = newLeft
                                     }
           }

-- |Register handlers to be invoked when the edit widget has been
-- ''activated'' (when the user presses Enter while the widget is
-- focused).  These handlers will only be invoked when a single-line
-- edit widget is activated; multi-line widgets never generate these
-- events.
onActivate :: Widget Edit -> (Widget Edit -> IO ()) -> IO ()
onActivate = addHandler (activateHandlers <~~)

notifyActivateHandlers :: Widget Edit -> IO ()
notifyActivateHandlers wRef = fireEvent wRef (activateHandlers <~~) wRef

notifyChangeHandlers :: Widget Edit -> IO ()
notifyChangeHandlers wRef = do
  s <- getEditText wRef
  fireEvent wRef (changeHandlers <~~) s

notifyCursorMoveHandlers :: Widget Edit -> IO ()
notifyCursorMoveHandlers wRef = do
  pos <- getEditCursorPosition wRef
  fireEvent wRef (cursorMoveHandlers <~~) pos

-- |Register handlers to be invoked when the edit widget's contents
-- change.  Handlers will be passed the new contents.
onChange :: Widget Edit -> (T.Text -> IO ()) -> IO ()
onChange = addHandler (changeHandlers <~~)

-- |Register handlers to be invoked when the edit widget's cursor
-- position changes.  Handlers will be passed the new cursor position,
-- relative to the beginning of the text (position (0, 0)).
onCursorMove :: Widget Edit -> ((Int, Int) -> IO ()) -> IO ()
onCursorMove = addHandler (cursorMoveHandlers <~~)

-- |Get the current contents of the edit widget.  This returns all of
-- the lines of text in the widget, separated by newlines.
getEditText :: Widget Edit -> IO T.Text
getEditText = (((T.intercalate (T.pack "\n")) . Z.getText . contents) <~~)

-- |Get the contents of the current line of the edit widget (the line
-- on which the cursor is positioned).
getEditCurrentLine :: Widget Edit -> IO T.Text
getEditCurrentLine = ((Z.currentLine . contents) <~~)

-- |Set the contents of the edit widget.  Newlines will be used to
-- break up the text in multiline widgets.  If the edit widget has a
-- line limit, only those lines within the limit will be set.  If the edit
-- widget has a line length limit, lines will be truncated.
setEditText :: Widget Edit -> T.Text -> IO ()
setEditText wRef str = do
  lim <- lineLimit <~~ wRef
  maxL <- maxLength <~~ wRef
  let ls1 = case lim of
             Nothing -> T.lines str
             Just l -> take l $ T.lines str
      ls2 = case maxL of
             Nothing -> ls1
             Just l -> ((T.take l) <$>) $ T.lines str
  updateWidgetState wRef $ \st -> st { contents = Z.textZipper ls2
                                     }
  notifyChangeHandlers wRef

-- |Get the edit widget's current cursor position (row, column).
getEditCursorPosition :: Widget Edit -> IO (Int, Int)
getEditCursorPosition = ((Z.cursorPosition . contents) <~~)

-- |Set the cursor position to the specified row and column.  Invalid
-- cursor positions will be ignored.
setEditCursorPosition :: (Int, Int) -> Widget Edit -> IO ()
setEditCursorPosition pos = applyEdit (Z.moveCursor pos)

-- |Compute the physical cursor position (column) for the cursor in a
-- given edit widget state.  The physical position is relative to the
-- beginning of the current line (i.e., zero, as opposed to the
-- displayStart and related state).
physCursorCol :: Edit -> Phys
physCursorCol s =
    let curLine = T.unpack $ Z.currentLine $ contents s
        (_, cursorColumn) = Z.cursorPosition $ contents s
    in toPhysical cursorColumn curLine

-- |Apply an editing transformation to the edit widget's text.  If the
-- transformation modifies the text or the cursor, the appropriate
-- event handlers will be notified.  If a line limit is in effect and
-- the transformation violates it, the transformation will be ignored.
applyEdit :: (Z.TextZipper T.Text -> Z.TextZipper T.Text)
          -> Widget Edit
          -> IO ()
applyEdit f this = do
  old <- contents <~~ this
  llim <- lineLimit <~~ this
  maxL <- maxLength <~~ this

  let checkLines tz = case llim of
                        Nothing -> Just tz
                        Just l -> if length (Z.getText tz) > l
                                  then Nothing
                                  else Just tz
      checkLength tz = case maxL of
                         Nothing -> Just tz
                         Just l -> if or ((> l) <$> (Z.lineLengths tz))
                                   then Nothing
                                   else Just tz
      newContents = checkLength =<< checkLines (f old)

  when (isJust newContents) $ do
    let Just new = newContents
    updateWidgetState this $ \s -> s { contents = new }

    when (Z.getText old /= Z.getText new) $
         notifyChangeHandlers this

    when (Z.cursorPosition old /= Z.cursorPosition new) $
         notifyCursorMoveHandlers this

editKeyEvent :: Widget Edit -> Key -> [Modifier] -> IO Bool
editKeyEvent this k mods = do
  let run f = applyEdit f this >> return True
  case (k, mods) of
    (KChar 'a', [MCtrl]) -> run Z.gotoBOL
    (KChar 'k', [MCtrl]) -> run Z.killToEOL
    (KChar 'e', [MCtrl]) -> run Z.gotoEOL
    (KChar 'd', [MCtrl]) -> run Z.deleteChar
    (KLeft, []) -> run Z.moveLeft
    (KRight, []) -> run Z.moveRight
    (KUp, []) -> run Z.moveUp
    (KDown, []) -> run Z.moveDown
    (KBS, []) -> do
        v <- run Z.deletePrevChar
        when (v) $ do
            -- We want deletions to cause content earlier on the line(s) to
            -- slide in from the left rather than letting the cursor reach the
            -- beginning of the edit widget, preventing the user from seeing
            -- characters being deleted.  To do this, after deletion (if we
            -- can) we slide the clipping window one column to the left.
            updateWidgetState this $ \st ->
                let r = clipRect st
                in if clipLeft r > 0
                   then st { clipRect = r { clipLeft = clipLeft r - Phys 1 } }
                   else st
        return v
    (KDel, []) -> run Z.deleteChar
    (KChar ch, []) -> do
        st <- getState this
        if charFilter st ch
            then run (Z.insertChar ch)
            else return True -- True even if the filter rejected the character.
    (KHome, []) -> run Z.gotoBOL
    (KEnd, []) -> run Z.gotoEOL
    (KEnter, []) -> do
                   lim <- lineLimit <~~ this
                   case lim of
                     Just 1 -> notifyActivateHandlers this >> return True
                     _ -> run Z.breakLine
    _ -> return False
