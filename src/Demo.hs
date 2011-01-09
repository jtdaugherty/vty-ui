{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All

-- The application state; this contains references to widgets that
-- need to be updated when events occur.
data AppState =
    AppState { theList :: Widget (List String FormattedText)
             , theBody :: Widget FormattedText
             , theFooter1 :: Widget FormattedText
             , theFooter2 :: Widget FormattedText
             , theEdit :: Widget Edit
             , theListLimit :: Widget (VLimit (List String FormattedText))
             , uis :: Widget Collection
             , theTable :: Widget Table
             }

-- Visual attributes.
titleAttr = bright_white `on` blue
editAttr = white `on` black
editFocusAttr = bright_green `on` black
boxAttr = bright_yellow `on` black
bodyAttr = bright_green `on` black
selAttr = black `on` yellow
hlAttr1 = red `on` black
hlAttr2 = yellow `on` black

uiCore appst w = do
  (hBorder titleAttr)
      <--> w
      <--> ((return $ theFooter1 appst)
            <++> (return $ theFooter2 appst)
            <++> hBorder titleAttr)

buildUi1 appst = do
  c <- hCentered $ theTable appst
  uiCore appst (bottomPadded c bodyAttr)

buildUi2 appst =
    uiCore appst ((return $ theListLimit appst)
                  <--> (hBorder titleAttr)
                  <--> (bottomPadded (theBody appst) bodyAttr))

-- Construct the application state using the message map.
mkAppState :: IO AppState
mkAppState = do
  lw <- mkSimpleList bodyAttr selAttr []
  b <- textWidget wrap $ prepareText bodyAttr ""
  f1 <- simpleText titleAttr ""
  f2 <- simpleText titleAttr "[]"
  e <- editWidget editAttr editFocusAttr
  ll <- vLimit 5 lw
  t <- newTable bodyAttr [Fixed 10, Fixed 20] (BorderPartial [Rows, Columns, Edges])

  c <- newCollection

  return $ AppState { theList = lw
                    , theBody = b
                    , theFooter1 = f1
                    , theFooter2 = f2
                    , theEdit = e
                    , theListLimit = ll
                    , uis = c
                    , theTable = t
                    }

exitApp :: Vty -> IO a
exitApp vty = do
  reserve_display $ terminal vty
  shutdown vty
  exitSuccess

updateBody :: AppState -> Widget (List a b) -> IO ()
updateBody st w = do
  result <- getSelected w
  let msg = case result of
              Nothing -> ""
              Just (i, _) -> "This is the text for list entry " ++ (show $ i + 1)
  setText (theBody st) msg bodyAttr

updateFooterNums :: AppState -> Widget (List a b) -> IO ()
updateFooterNums st w = do
  result <- getSelected w
  sz <- getListSize w
  let msg = case result of
              Nothing -> "--/--"
              Just (i, _) ->
                  "-" ++ (show $ i + 1) ++ "/" ++
                          (show sz) ++ "-"
  setText (theFooter1 st) msg titleAttr

updateFooterText :: AppState -> Widget Edit -> IO ()
updateFooterText st w = do
  t <- getEditText w
  setText (theFooter2 st) ("[" ++ t ++ "]") titleAttr

main :: IO ()
main = do
  vty <- mkVty

  st <- mkAppState

  ui1 <- buildUi1 st
  ui2 <- buildUi2 st

  showMainUI <- addToCollection (uis st) ui1
  showMessageUI <- addToCollection (uis st) ui2

  fg1 <- newFocusGroup
  listCtx1 <- addToFocusGroup fg1 (theList st)
  addToFocusGroup_ fg1 (theEdit st)

  fg2 <- newFocusGroup
  listCtx2 <- addToFocusGroup fg2 (theList st)
  addToFocusGroup_ fg2 (theEdit st)

  -- These event handlers will fire regardless of the input event
  -- context.
  (theEdit st) `onChange` (updateFooterText st)
  (theEdit st) `onActivate` \e -> do
         addToList (theList st) =<< getEditText e
         setEditText e ""

  (theList st) `onSelectionChange` (updateBody st)
  (theList st) `onSelectionChange` (updateFooterNums st)
  (theList st) `onItemAdded` (\l _ _ _ -> updateFooterNums st l)
  (theList st) `onItemRemoved` (\l _ _ _ -> updateFooterNums st l)

  (theList st) `onKeyPressed` \_ k _ -> do
         case k of
           (KASCII 'q') -> exitApp vty
           KDel -> do
                  result <- getSelected (theList st)
                  case result of
                    Nothing -> return ()
                    Just (i, _) -> removeFromList (theList st) i >> return ()
                  return True
           _ -> return False

  -- These event handlers will only fire when the UI is in the
  -- appropriate mode, depending on the state of the Widget
  -- Collection.
  listCtx1 `onKeyPressed` \_ k _ -> do
            case k of
              KEnter -> do
                     r <- getSelected (theList st)
                     case r of
                       Nothing -> return True
                       Just _ -> showMessageUI >> return True
              _ -> return False

  listCtx2 `onKeyPressed` \_ k _ -> do
         case k of
           KASCII 'c' -> showMainUI >> return True
           KASCII '+' -> do
                  addToVLimit (theListLimit st) 1
                  return True
           KASCII '-' -> do
                  addToVLimit (theListLimit st) (-1)
                  return True
           _ -> return False

  setFocusGroup ui1 fg1
  setFocusGroup ui2 fg2

  setEditText (theEdit st) "edit me"

  addHeadingRow_ (theTable st) (bright_yellow `on` black) ["Static", "Editable"]

  w1 <- hCentered =<< simpleText bodyAttr "Foobar"
  addRow (theTable st) [ mkCell w1
                       , mkCell $ theEdit st
                       ]

  -- We need to call these handlers manually because while they will
  -- be called automatically as items are added to the list in the
  -- future, the items currently in the list didn't call these because
  -- they weren't registered at the time the items were added.  And
  -- that was impossible because the list was created and populated
  -- before we even got a reference to it, so we couldn't have set up
  -- event handlers.
  updateFooterNums st (theList st)
  updateBody st (theList st)

  -- Enter the event loop.
  runUi vty (uis st)
