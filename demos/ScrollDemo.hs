{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad (forM_)
import System.Exit
import qualified Data.Text as T
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.VScroll

buildUi = do
  let columns = [ column (ColFixed 15) `pad` (padAll 1)
                , column (ColFixed 15) `pad` (padAll 1)
                ]

  table <- newTable columns BorderFull >>=
           withNormalAttribute (bgColor blue) >>=
           withBorderAttribute (fgColor green)

  addHeadingRow_ table (yellow `on` blue) ["Column 1", "Column 2"]
  forM_ [1..50] $ \(i::Int) -> do
      tLeft <- plainText $ T.pack $ "Left (" ++ show i ++ ")"
      tRight <- plainText $ T.pack $ "Right (" ++ show i ++ ")"
      addRow table $ tLeft .|. tRight

  centered =<< hLimit 55 =<<
    ((vLimit 25 =<< vScroll table) >>= bordered)

main :: IO ()
main = do
  c <- newCollection
  ui <- buildUi
  fg <- newFocusGroup

  _ <- addToCollection c ui fg
  _ <- addToFocusGroup fg ui

  fg `onKeyPressed` \_ k _ -> do
         case k of
           (KChar 'q') -> exitSuccess
           _ -> return False

  runUi c $ defaultContext { normalAttr = white `on` black
                           , focusAttr = white `on` blue
                           }
