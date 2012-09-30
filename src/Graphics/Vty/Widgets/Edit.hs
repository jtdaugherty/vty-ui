{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
    , setEditCursorPosition
    , getEditCursorPosition
    , setEditLineLimit
    , getEditLineLimit
    , onActivate
    , onChange
    , onCursorMove
    )
where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.Text as T
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util

data Edit = Edit { currentText :: [T.Text]
                 , cursorRow :: Int
                 , cursorColumn :: Int
                 , displayStart :: Int
                 , displayWidth :: Int
                 , topRow :: Int
                 , visibleRows :: Int
                 , activateHandlers :: Handlers (Widget Edit)
                 , changeHandlers :: Handlers T.Text
                 , cursorMoveHandlers :: Handlers (Int, Int)
                 , lineLimit :: Maybe Int
                 }

instance Show Edit where
    show e = concat [ "Edit { "
                    , "currentText = ", show $ currentText e
                    , ", cursorColumn = ", show $ cursorColumn e
                    , ", cursorRow = ", show $ cursorRow e
                    , ", topRow = ", show $ topRow e
                    , ", lineLimit = ", show $ lineLimit e
                    , ", visibleRows = ", show $ visibleRows e
                    , ", displayStart = ", show $ displayStart e
                    , ", displayWidth = ", show $ displayWidth e
                    , " }"
                    ]

editWidget' :: IO (Widget Edit)
editWidget' = do
  ahs <- newHandlers
  chs <- newHandlers
  cmhs <- newHandlers

  let initSt = Edit { currentText = [T.empty]
                    , cursorRow = 0
                    , cursorColumn = 0
                    , displayStart = 0
                    , displayWidth = 0
                    , topRow = 0
                    , visibleRows = 1
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
              f <- focused <~ this
              pos <- getCurrentPosition this
              curRow <- cursorRow <~~ this
              curCol <- cursorColumn <~~ this
              startCol <- displayStart <~~ this
              startRow <- topRow <~~ this

              if f then
                  return (Just $ pos `plusWidth` (toEnum (curCol - startCol)) `plusHeight` (toEnum (curRow - startRow))) else
                  return Nothing

        , render_ =
            \this size ctx -> do
              resize this ( fromEnum $ region_height size
                          , fromEnum $ region_width size )

              st <- getState this

              let truncated l = T.take (displayWidth st)
                                (T.drop (displayStart st) l)

                  visibleLines = take (visibleRows st) $
                                 drop (topRow st) (currentText st)
                  truncatedLines = truncated <$> visibleLines

                  nAttr = mergeAttrs [ overrideAttr ctx
                                     , normalAttr ctx
                                     ]

                  totalAllowedLines = fromEnum $ region_height size
                  numEmptyLines = lim - length truncatedLines
                      where
                        lim = case lineLimit st of
                                Just v -> min v totalAllowedLines
                                Nothing -> totalAllowedLines

                  emptyLines = replicate numEmptyLines T.empty

              isFocused <- focused <~ this
              let attr = if isFocused then focusAttr ctx else nAttr
                  lineWidget s = string attr (T.unpack s)
                                 <|> char_fill attr ' ' (region_width size - (toEnum $ T.length s)) 1

              return $ vert_cat $ lineWidget <$> (truncatedLines ++ emptyLines)

        , keyEventHandler = editKeyEvent
        }
  return wRef

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

resize :: Widget Edit -> (Int, Int) -> IO ()
resize e (newHeight, newWidth) = do
  updateWidgetState e $ \st -> st { visibleRows = newHeight }
  setDisplayWidth e newWidth

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
getEditText = (((T.intercalate (T.pack "\n")) . currentText) <~~)

-- |Get the contents of the current line of the edit widget (the line
-- on which the cursor is positioned).
getEditCurrentLine :: Widget Edit -> IO T.Text
getEditCurrentLine e = do
  ls <- currentText <~~ e
  curL <- cursorRow <~~ e
  return $ ls !! curL

setEditCurrentLine :: Widget Edit -> T.Text -> IO ()
setEditCurrentLine e s = do
  ls <- currentText <~~ e
  curL <- cursorRow <~~ e

  updateWidgetState e $ \st ->
      st { currentText = repl curL s ls
         }

-- |Set the contents of the edit widget.  Newlines will be used to
-- break up the text in multiline widgets.  If the edit widget has a
-- line limit, only those lines within the limit will be set.
setEditText :: Widget Edit -> T.Text -> IO ()
setEditText wRef str = do
  oldS <- currentText <~~ wRef
  lim <- lineLimit <~~ wRef
  s <- case lim of
    Nothing -> return str
    Just l -> return $ T.intercalate (T.pack "\n") $ take l $ T.lines str
  updateWidgetState wRef $ \st -> st { currentText = if T.null s
                                                     then [T.empty]
                                                     else T.lines s
                                     , cursorColumn = 0
                                     , cursorRow = 0
                                     }
  when (oldS /= T.lines s) $ do
    gotoBeginning wRef
    notifyChangeHandlers wRef

-- |Set the current edit widget cursor position.  The tuple is (row,
-- column) with each starting at zero.  Invalid cursor positions will
-- be ignored.
setEditCursorPosition :: Widget Edit -> (Int, Int) -> IO ()
setEditCursorPosition wRef (newRow, newCol) = do
  ls <- currentText <~~ wRef

  -- First, check that the row is valid
  case newRow >= 0 && newRow < (length ls) of
    False -> return ()
    True -> do
      -- Then, if the row is valid, is the column valid for that row?
      -- It's legal for the new position to be *after* the last
      -- character (i.e., in the case of go-to-end)
      case newCol >= 0 && newCol <= (T.length (ls !! newRow)) of
        False -> return ()
        True -> do
              (oldRow, oldCol) <- getEditCursorPosition wRef
              when ((newRow, newCol) /= (oldRow, oldCol)) $
                   do
                     st <- getState wRef

                     let newDisplayStart = if newCol >= (displayStart st + displayWidth st)
                                           then newCol - displayWidth st + 1
                                           else if newCol < displayStart st
                                                then newCol
                                                else displayStart st
                         newTopRow = if newRow < topRow st
                                     then newRow
                                     else if newRow >= (topRow st + visibleRows st)
                                          then newRow - visibleRows st + 1
                                          else topRow st

                     updateWidgetState wRef $ \s ->
                         s { displayStart = newDisplayStart
                           , topRow = newTopRow
                           }

                     updateWidgetState wRef $ \s ->
                         s { cursorRow = newRow
                           , cursorColumn = newCol
                           }
                     notifyCursorMoveHandlers wRef

-- |Get the edit widget's current cursor position (row, column).
getEditCursorPosition :: Widget Edit -> IO (Int, Int)
getEditCursorPosition e = do
  r <- cursorRow <~~ e
  c <- cursorColumn <~~ e
  return (r, c)

setDisplayWidth :: Widget Edit -> Int -> IO ()
setDisplayWidth this width =
    updateWidgetState this $ \s ->
        let newDispStart = if cursorColumn s - displayStart s >= width
                           then cursorColumn s - width + 1
                           else displayStart s
        in s { displayWidth = width
             , displayStart = newDispStart
             }

editKeyEvent :: Widget Edit -> Key -> [Modifier] -> IO Bool
editKeyEvent this k mods = do
  case (k, mods) of
    (KASCII 'a', [MCtrl]) -> gotoBeginning this >> return True
    (KASCII 'k', [MCtrl]) -> killToEOL this >> return True
    (KASCII 'e', [MCtrl]) -> gotoEnd this >> return True
    (KASCII 'd', [MCtrl]) -> delCurrentChar this >> return True
    (KLeft, []) -> moveCursorLeft this >> return True
    (KRight, []) -> moveCursorRight this >> return True
    (KUp, []) -> moveCursorUp this >> return True
    (KDown, []) -> moveCursorDown this >> return True
    (KBS, []) -> deletePreviousChar this >> return True
    (KDel, []) -> delCurrentChar this >> return True
    (KASCII ch, []) -> insertChar this ch >> return True
    (KHome, []) -> gotoBeginning this >> return True
    (KEnd, []) -> gotoEnd this >> return True
    (KEnter, []) -> do
                   lim <- lineLimit <~~ this
                   case lim of
                     Just 1 -> notifyActivateHandlers this >> return True
                     _ -> insertLineAtPoint this >> return True
    _ -> return False

insertLineAtPoint :: Widget Edit -> IO ()
insertLineAtPoint e = do
  -- Bail if adding a new line would violate the line limit
  lim <- lineLimit <~~ e
  numLines <- (length . currentText) <~~ e

  let continue = case lim of
                   Just v | numLines + 1 > v -> False
                   _ -> True

  when continue $
       do
         -- Get information about current line so we can break the
         -- current line
         curL <- getEditCurrentLine e
         curCol <- cursorColumn <~~ e
         curRow <- cursorRow <~~ e
         let r1 = T.take curCol curL
             r2 = T.drop curCol curL
         setEditCurrentLine e r1
         updateWidgetState e $ \st ->
             st { currentText = inject (curRow + 1) r2 (currentText st)
                }
         notifyChangeHandlers e
         setEditCursorPosition e (curRow + 1, 0)

killToEOL :: Widget Edit -> IO ()
killToEOL this = do
  -- Preserve some state since setEditText changes it.
  curCol <- cursorColumn <~~ this
  curLine <- getEditCurrentLine this
  case T.null curLine of
    False -> do
      setEditCurrentLine this $ T.take curCol curLine
      notifyChangeHandlers this
    True -> do
      curRow <- cursorRow <~~ this
      numLines <- (length . currentText) <~~ this
      if curRow == 0 && numLines == 1 then
          return () else
          do
            let newRow = if curRow == numLines - 1 && numLines > 1
                         then curRow - 1
                         else curRow
            updateWidgetState this $ \st ->
                st { currentText = remove curRow (currentText st)
                   }
            notifyChangeHandlers this
            setEditCursorPosition this (newRow, 0)

deletePreviousChar :: Widget Edit -> IO ()
deletePreviousChar this = do
  curCol <- cursorColumn <~~ this
  curRow <- cursorRow <~~ this
  case curCol == 0 of
    True ->
        if curRow == 0
        then return ()
        else do
          curLine <- getEditCurrentLine this
          ls <- currentText <~~ this
          let prevLine = ls !! (curRow - 1)
          updateWidgetState this $ \st ->
              st { currentText = repl (curRow - 1) (T.concat [prevLine, curLine])
                                 $ remove curRow (currentText st)
                 }
          setEditCursorPosition this (curRow - 1, T.length prevLine)
          notifyChangeHandlers this

    False -> do
      moveCursorLeft this
      delCurrentChar this

gotoBeginning :: Widget Edit -> IO ()
gotoBeginning wRef = do
  updateWidgetState wRef $ \s -> s { displayStart = 0
                                   }
  curL <- cursorRow <~~ wRef
  setEditCursorPosition wRef (curL, 0)

gotoEnd :: Widget Edit -> IO ()
gotoEnd wRef = do
  curLine <- getEditCurrentLine wRef
  curRow <- cursorRow <~~ wRef
  setEditCursorPosition wRef (curRow, T.length curLine)

moveCursorUp :: Widget Edit -> IO ()
moveCursorUp wRef = do
  st <- getState wRef
  let newRow = if cursorRow st == 0
               then 0
               else cursorRow st - 1

      prevLine = currentText st !! (cursorRow st - 1)
      newCol = if cursorRow st == 0 || (cursorColumn st <= T.length prevLine)
               then cursorColumn st
               else T.length prevLine

  setEditCursorPosition wRef (newRow, newCol)

moveCursorDown :: Widget Edit -> IO ()
moveCursorDown wRef = do
  st <- getState wRef
  let newRow = if cursorRow st == (length $ currentText st) - 1
               then (length $ currentText st) - 1
               else cursorRow st + 1

      nextLine = currentText st !! (cursorRow st + 1)
      newCol = if cursorRow st == (length $ currentText st) - 1
               then cursorColumn st
               else if cursorColumn st <= T.length nextLine
                    then cursorColumn st
                    else T.length nextLine

  setEditCursorPosition wRef (newRow, newCol)

moveCursorLeft :: Widget Edit -> IO ()
moveCursorLeft wRef = do
  st <- getState wRef
  let newRow = if cursorRow st == 0
               then 0
               else if cursorColumn st == 0
                    then cursorRow st - 1
                    else cursorRow st
      prevLine = currentText st !! (cursorRow st - 1)
      newCol = if cursorColumn st == 0
               then if cursorRow st == 0
                    then 0
                    else T.length prevLine
               else cursorColumn st - 1
  setEditCursorPosition wRef (newRow, newCol)

moveCursorRight :: Widget Edit -> IO ()
moveCursorRight wRef = do
  st <- getState wRef
  curL <- getEditCurrentLine wRef
  let newRow = if cursorRow st == (length $ currentText st) - 1
               then cursorRow st
               else if cursorColumn st == T.length curL
                    then cursorRow st + 1
                    else cursorRow st
      newCol = if cursorColumn st == T.length curL
               then if cursorRow st == (length $ currentText st) - 1
                    then cursorColumn st
                    else 0
               else cursorColumn st + 1
  setEditCursorPosition wRef (newRow, newCol)

insertChar :: Widget Edit -> Char -> IO ()
insertChar wRef ch = do
  curLine <- getEditCurrentLine wRef
  updateWidgetState wRef $ \st ->
      let newLine = T.concat [ T.take (cursorColumn st) curLine
                             , T.singleton ch
                             , T.drop (cursorColumn st) curLine
                             ]
          newViewStart =
              if cursorColumn st == displayStart st + displayWidth st - 1
              then displayStart st + 1
              else displayStart st
      in st { displayStart = newViewStart
            , currentText = repl (cursorRow st) newLine (currentText st)
            }
  moveCursorRight wRef
  notifyChangeHandlers wRef

delCurrentChar :: Widget Edit -> IO ()
delCurrentChar wRef = do
  st <- getState wRef
  curLine <- getEditCurrentLine wRef
  case cursorColumn st < (T.length curLine) of
    True ->
        do
          let newLine = T.concat [ T.take (cursorColumn st) curLine
                                 , T.drop (cursorColumn st + 1) curLine
                                 ]
          updateWidgetState wRef $ \s -> s { currentText = repl (cursorRow st) newLine (currentText st) }
          notifyChangeHandlers wRef
    False ->
        -- If we are on the last line, do nothing, but if we aren't,
        -- combine the next line with the current one
        if cursorRow st == (length $ currentText st) - 1
        then return ()
        else do
          let nextLine = currentText st !! (cursorRow st + 1)
          updateWidgetState wRef $ \s ->
              s { currentText = remove (cursorRow s + 1) $
                                repl (cursorRow st) (T.concat [curLine, nextLine]) (currentText s)
                }