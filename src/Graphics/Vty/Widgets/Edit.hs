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
module Graphics.Vty.Widgets.Edit
    ( Edit
    , editWidget
    , multiLineEditWidget
    , getEditText
    , getEditCurrentLine
    , setEditText
    , getEditCursorPosition
    , setEditCursorPosition
    , setEditLineLimit
    , getEditLineLimit
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
import qualified Data.Text as T
import Graphics.Vty
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
                    }

  wRef <- newWidget initSt $ \w ->
      w { growHorizontal_ = const $ return True
        , growVertical_ =
            \this -> do
              case lineLimit this of
                Just v | v == 1 -> return False
                _ -> return True

        , getCursorPosition_ =
            \this -> do
              st <- getState this
              f <- focused <~ this
              pos <- getCurrentPosition this

              let (cursorRow, _) = Z.cursorPosition (contents st)
                  Phys offset = physCursorCol st - clipLeft (clipRect st)
                  newPos = pos
                           `withWidth` (toEnum ((fromEnum $ region_width pos) + offset))
                           `plusHeight` (toEnum (cursorRow - (fromEnum $ clipTop $ clipRect st)))

              return $ if f then Just newPos else Nothing

        , render_ =
            \this size ctx -> do
              resize this ( Phys $ fromEnum $ region_height size
                          , Phys $ fromEnum $ region_width size )

              st <- getState this

              let nAttr = mergeAttrs [ overrideAttr ctx
                                     , normalAttr ctx
                                     ]

                  clipped = doClipping (Z.getText $ contents st) (clipRect st)

                  totalAllowedLines = fromEnum $ region_height size
                  numEmptyLines = lim - length clipped
                      where
                        lim = case lineLimit st of
                                Just v -> min v totalAllowedLines
                                Nothing -> totalAllowedLines

                  emptyLines = replicate numEmptyLines ""

              isFocused <- focused <~ this
              let attr = if isFocused then focusAttr ctx else nAttr
                  lineWidget s = let Phys physLineLength = sum $ chWidth <$> s
                                 in string attr s <|>
                                    char_fill attr ' ' (region_width size - toEnum physLineLength) 1

              return $ vert_cat $ lineWidget <$> (clipped ++ emptyLines)

        , keyEventHandler = editKeyEvent
        }
  return wRef

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

-- |Set the limit on the number of lines for the edit widget.  Nothing
-- indicates no limit, while Just indicates a limit of the specified
-- number of lines.
setEditLineLimit :: Widget Edit -> Maybe Int -> IO ()
setEditLineLimit _ (Just v) | v <= 0 = return ()
setEditLineLimit w v = updateWidgetState w $ \st -> st { lineLimit = v }

-- |Get the current line limit, if any, for the edit widget.
getEditLineLimit :: Widget Edit -> IO (Maybe Int)
getEditLineLimit = (lineLimit <~~)

resize :: Widget Edit -> (Phys, Phys) -> IO ()
resize e (newHeight, newWidth) = do
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
-- relative to the beginning of the string (position 0).
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
-- line limit, only those lines within the limit will be set.
setEditText :: Widget Edit -> T.Text -> IO ()
setEditText wRef str = do
  lim <- lineLimit <~~ wRef
  let ls = case lim of
             Nothing -> T.lines str
             Just l -> take l $ T.lines str
  updateWidgetState wRef $ \st -> st { contents = Z.textZipper ls
                                     }
  notifyChangeHandlers wRef

-- |Get the edit widget's current cursor position (row, column).
getEditCursorPosition :: Widget Edit -> IO (Int, Int)
getEditCursorPosition = ((Z.cursorPosition . contents) <~~)

-- |Set the cursor position to the specified row and column.  Invalid
-- cursor positions will be ignored.
setEditCursorPosition :: (Int, Int) -> Widget Edit -> IO ()
setEditCursorPosition pos w = applyEdit w (Z.moveCursor pos)

-- |Compute the physical cursor position (column) for the cursor in a
-- given edit widget state.  The physical position is relative to the
-- beginning of the current line (i.e., zero, as opposed to the
-- displayStart and related state).
physCursorCol :: Edit -> Phys
physCursorCol s =
    let curLine = T.unpack $ Z.currentLine $ contents s
        (_, cursorColumn) = Z.cursorPosition $ contents s
    in toPhysical cursorColumn curLine

applyEdit :: Widget Edit
          -> (Z.TextZipper T.Text -> Z.TextZipper T.Text)
          -> IO ()
applyEdit this f = do
  oldC <- contents <~~ this
  updateWidgetState this $ \s ->
      let newSt = s { contents = f (contents s) }
      in case lineLimit s of
           Nothing -> newSt
           Just l -> if length (Z.getText $ contents newSt) > l
                     then s
                     else newSt

  newC <- contents <~~ this

  when (Z.getText oldC /= Z.getText newC) $
       notifyChangeHandlers this

  when (Z.cursorPosition oldC /= Z.cursorPosition newC) $
       notifyCursorMoveHandlers this

editKeyEvent :: Widget Edit -> Key -> [Modifier] -> IO Bool
editKeyEvent this k mods = do
  case (k, mods) of
    (KASCII 'a', [MCtrl]) -> applyEdit this Z.gotoBOL >> return True
    (KASCII 'k', [MCtrl]) -> applyEdit this Z.killToEOL >> return True
    (KASCII 'e', [MCtrl]) -> applyEdit this Z.gotoEOL >> return True
    (KASCII 'd', [MCtrl]) -> applyEdit this Z.deleteChar >> return True
    (KLeft, []) -> applyEdit this Z.moveLeft >> return True
    (KRight, []) -> applyEdit this Z.moveRight >> return True
    (KUp, []) -> applyEdit this Z.moveUp >> return True
    (KDown, []) -> applyEdit this Z.moveDown >> return True
    (KBS, []) -> applyEdit this Z.deletePrevChar >> return True
    (KDel, []) -> applyEdit this Z.deleteChar >> return True
    (KASCII ch, []) -> applyEdit this (Z.insertChar ch) >> return True
    (KHome, []) -> applyEdit this Z.gotoBOL >> return True
    (KEnd, []) -> applyEdit this Z.gotoEOL >> return True
    (KEnter, []) -> do
                   lim <- lineLimit <~~ this
                   case lim of
                     Just 1 -> notifyActivateHandlers this >> return True
                     _ -> applyEdit this Z.breakLine >> return True
    _ -> return False
