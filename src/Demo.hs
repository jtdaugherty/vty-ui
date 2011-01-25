{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
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
             }

-- Visual attributes.
titleAttr = bright_white `on` blue
editAttr = white `on` black
focAttr = black `on` green
boxAttr = bright_yellow `on` black
bodyAttr = bright_green `on` black
selAttr = black `on` yellow
hlAttr1 = red `on` black
hlAttr2 = yellow `on` black

uiCore appst w = do
  (hBorder >>= withBorderAttribute titleAttr)
      <--> w
      <--> (hBorder >>= withBorderAttribute titleAttr)
      <--> (return $ theEdit appst)
      <--> ((return $ theFooter1 appst)
            <++> (return $ theFooter2 appst)
            <++> (hBorder >>= withBorderAttribute titleAttr))

buildUi1 appst = do
  uiCore appst (return $ theList appst)

buildUi2 appst =
    uiCore appst ((return $ theListLimit appst)
                  <--> (hBorder >>= withBorderAttribute titleAttr)
                  <--> (bottomPadded (theBody appst) bodyAttr))

-- Construct the application state using the message map.
mkAppState :: IO AppState
mkAppState = do
  lw <- mkSimpleList selAttr []
  b <- textWidget wrap $ prepareText def_attr ""
  f1 <- simpleText "" >>= withNormalAttribute titleAttr
  f2 <- simpleText "[]" >>= withNormalAttribute titleAttr
  e <- editWidget
  ll <- vLimit 5 lw

  c <- newCollection

  return $ AppState { theList = lw
                    , theBody = b
                    , theFooter1 = f1
                    , theFooter2 = f2
                    , theEdit = e
                    , theListLimit = ll
                    , uis = c
                    }

updateBody :: AppState -> Int -> IO ()
updateBody st i = do
  let msg = "This is the text for list entry " ++ (show $ i + 1)
  setText (theBody st) msg

updateFooterNums :: AppState -> Widget (List a b) -> IO ()
updateFooterNums st w = do
  result <- getSelected w
  sz <- getListSize w
  let msg = case result of
              Nothing -> "--/--"
              Just (i, _) ->
                  "-" ++ (show $ i + 1) ++ "/" ++
                          (show sz) ++ "-"
  setText (theFooter1 st) msg

updateFooterText :: AppState -> Widget Edit -> String -> IO ()
updateFooterText st _ t = setText (theFooter2 st) ("[" ++ t ++ "]")

main :: IO ()
main = do
  st <- mkAppState

  ui1 <- buildUi1 st
  ui2 <- buildUi2 st

  showMainUI <- addToCollection (uis st) ui1
  showMessageUI <- addToCollection (uis st) ui2

  fg1 <- newFocusGroup
  listCtx1 <- addToFocusGroup fg1 (theList st)
  addToFocusGroup fg1 (theEdit st)

  fg2 <- newFocusGroup
  listCtx2 <- addToFocusGroup fg2 (theList st)
  addToFocusGroup fg2 (theEdit st)

  -- These event handlers will fire regardless of the input event
  -- context.
  (theEdit st) `onChange` (updateFooterText st)
  (theEdit st) `onActivate` \e -> do
         addToList (theList st) =<< getEditText e
         setEditText e ""

  (theList st) `onSelectionChange` \_ i _ _ -> updateBody st i
  (theList st) `onSelectionChange` \lst _ _ _ -> updateFooterNums st lst
  (theList st) `onItemAdded` (\l _ _ _ -> updateFooterNums st l)
  (theList st) `onItemRemoved` (\l _ _ _ -> updateFooterNums st l)

  (theList st) `onKeyPressed` \_ k _ -> do
         case k of
           (KASCII 'q') -> exitSuccess
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

  -- We need to call these handlers manually because while they will
  -- be called automatically as items are added to the list in the
  -- future, the items currently in the list didn't call these because
  -- they weren't registered at the time the items were added.  And
  -- that was impossible because the list was created and populated
  -- before we even got a reference to it, so we couldn't have set up
  -- event handlers.
  updateFooterNums st (theList st)

  -- Enter the event loop.
  runUi (uis st) $ defaultContext { normalAttr = bodyAttr
                                  , focusAttr = focAttr
                                  }
