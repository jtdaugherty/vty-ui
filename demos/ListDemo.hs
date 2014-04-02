{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit ( exitSuccess )
import qualified Data.Text as T
import Graphics.Vty
import Graphics.Vty.Widgets.All

data AppElements =
    AppElements { theList :: Widget (List T.Text FormattedText)
                , theBody :: Widget FormattedText
                , theFooter :: Widget FormattedText
                , theListLimit :: Widget (VLimit (List T.Text FormattedText))
                , uis :: Collection
                }

titleAttr = bright_white `on` blue
focAttr = black `on` green
bodyAttr = white `on` black
selAttr = black `on` yellow
keyAttr = fgColor magenta

message1 :: T.Text
message1 = "This demonstration shows how list widgets behave. \n\
           \See the keystrokes below to try the demo."

message2 :: [(T.Text, Attr)]
message2 = [ ("- Press ", def_attr), ("q", keyAttr), (" to quit\n", def_attr)
           , ("- Press ", def_attr), ("+", keyAttr)
           , (" / ", def_attr), ("a", keyAttr)
           , (" to add a list item\n", def_attr)
           , ("- Press ", def_attr), ("-", keyAttr)
           , (" / ", def_attr), ("d", keyAttr)
           , (" to remove the selected list item\n", def_attr)
           , ("- Press ", def_attr)
           , ("up", keyAttr), (" / ", def_attr)
           , ("down", keyAttr), (" / ", def_attr)
           , ("page up", keyAttr), (" / ", def_attr)
           , ("page down", keyAttr)
           , (" to navigate the list\n", def_attr)
           ]

buildUi appst = do
  msg1 <- plainText message1
  setTextFormatter msg1 wrap

  msg2 <- plainTextWithAttrs message2
  setTextFormatter msg2 wrap

  mainUi <- (hBorder >>= withBorderAttribute titleAttr)
         <--> (return $ theList appst)
         <--> ((return $ theFooter appst)
               <++> (hBorder >>= withBorderAttribute titleAttr))

  centered =<< hLimit 55 =<<
             ((return msg1)
                  <--> ((vLimit 25 mainUi >>= bordered)
                        <--> return msg2 >>= withBoxSpacing 1)
                    >>= withBoxSpacing 1)

-- Construct the application statea using the message map.
mkAppElements :: IO AppElements
mkAppElements = do
  lw <- newTextList selAttr [] 1
  b <- textWidget wrap T.empty
  ft <- plainText T.empty >>= withNormalAttribute titleAttr
  ll <- vLimit 5 lw

  c <- newCollection

  return $ AppElements { theList = lw
                       , theBody = b
                       , theFooter = ft
                       , theListLimit = ll
                       , uis = c
                       }

updateBody :: AppElements -> Int -> IO ()
updateBody st i = do
  let msg = "This is the text for list entry " ++ (show $ i + 1)
  setText (theBody st) $ T.pack msg

updateFooterNums :: AppElements -> Widget (List a b) -> IO ()
updateFooterNums st w = do
  result <- getSelected w
  sz <- getListSize w
  let msg = case result of
              Nothing -> "0/0"
              Just (i, _) -> (show $ i + 1) ++ "/" ++ (show sz)
  setText (theFooter st) $ T.pack msg

main :: IO ()
main = do
  st <- mkAppElements

  ui <- buildUi st
  fg <- newFocusGroup

  _ <- addToCollection (uis st) ui fg
  _ <- addToFocusGroup fg (theList st)

  (theList st) `onSelectionChange` \_ -> updateFooterNums st $ theList st
  (theList st) `onItemAdded` \_ -> updateFooterNums st $ theList st
  (theList st) `onItemRemoved` \_ -> updateFooterNums st $ theList st

  let removeCurrentItem = do
         result <- getSelected (theList st)
         case result of
           Nothing -> return ()
           Just (i, _) -> removeFromList (theList st) i >> return ()
      addNewItem =
          (plainText "a list item") >>=
                    addToList (theList st) "unused"

  (theList st) `onKeyPressed` \_ k _ -> do
         case k of
           (KASCII 'q') -> exitSuccess
           (KASCII '-') -> removeCurrentItem >> return True
           (KASCII 'd') -> removeCurrentItem >> return True
           (KASCII '+') -> addNewItem >> return True
           (KASCII 'a') -> addNewItem >> return True
           _ -> return False

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
