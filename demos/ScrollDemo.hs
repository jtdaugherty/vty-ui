{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad (forM_, forM)
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
  es <- forM [1..50] $ \(i::Int) -> do
      tLeft <- plainText $ T.pack $ "Left (" ++ show i ++ ")"
      tRight <- editWidget
      addRow table $ tLeft .|. tRight
      return tRight

  w <- centered =<< hLimit 55 =<<
    ((vLimit 25 =<< vScroll table) >>= bordered)
  return (w, es)

main :: IO ()
main = do
  c <- newCollection
  (ui, es) <- buildUi
  fg <- newFocusGroup

  _ <- addToCollection c ui fg
  forM_ es $ addToFocusGroup fg

  fg `onKeyPressed` \_ k ms -> do
         case k of
           (KChar 'q') -> exitSuccess
           _ -> handleKeyEvent ui k ms

  runUi c $ defaultContext { normalAttr = white `on` black
                           , focusAttr = black `on` yellow
                           }
