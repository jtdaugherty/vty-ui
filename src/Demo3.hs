module Main where

import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  e <- editWidget
  ui <- centered e

  fg <- newFocusGroup
  addToFocusGroup fg e

  c <- newCollection
  addToCollection c ui fg

  e `onActivate` \this ->
      getEditText this >>= (error . ("You entered: " ++))

  runUi c defaultContext
