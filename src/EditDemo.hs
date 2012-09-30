module Main where

import qualified Data.Text as T
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
  e1 <- multiLineEditWidget
  e2 <- multiLineEditWidget
  setEditLineLimit e2 $ Just 3
  e3 <- editWidget

  fg <- newFocusGroup
  _ <- addToFocusGroup fg e1
  _ <- addToFocusGroup fg e2
  _ <- addToFocusGroup fg e3

  be1 <- bordered =<< boxFixed 40 5 e1
  be2 <- bordered =<< boxFixed 40 3 e2
  be3 <- bordered =<< boxFixed 40 1 e3

  c <- centered =<< ((plainText $ T.pack "Multi-Line Editor (unlimited lines):")
                         <--> (return be1)
                         <--> (plainText $ T.pack "Multi-Line Editor (3 lines):")
                         <--> (return be2)
                         <--> (plainText $ T.pack "Single-Line Editor:")
                         <--> (return be3)
                         <--> (plainText $ T.pack "- Esc to quit\n\n- TAB to switch editors") >>= withBoxSpacing 1
                    )

  coll <- newCollection
  _ <- addToCollection coll c fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> shutdownUi >> return True
        _ -> return False

  runUi coll $ defaultContext { focusAttr = fgColor yellow }
