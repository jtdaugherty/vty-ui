module Graphics.Vty.Widgets.EventLoop
    ( runUi
    )
where

import Data.Maybe
    ( isNothing
    )
import Control.Monad
    ( when
    )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Control.Exception
    ( finally
    )
import Graphics.Vty
import Graphics.Vty.Widgets.Core
    ( Widget
    , renderAndPosition
    , handleKeyEvent
    , getFocusGroup
    , getCursorPosition
    )

runUi :: Vty -> Widget a -> IO ()
runUi vty uiWidget =
    runUi' vty uiWidget `finally` do
      reserve_display $ terminal vty
      shutdown vty

runUi' :: Vty -> Widget a -> IO ()
runUi' vty uiWidget = do
  mFg <- getFocusGroup uiWidget
  when (isNothing mFg) $ error "fatal: top-level widget has no FocusGroup widget"

  let Just fg = mFg

  evt <- liftIO $ do
           sz <- display_bounds $ terminal vty
           img <- renderAndPosition uiWidget (DisplayRegion 0 0) sz Nothing
           update vty $ pic_for_image img

           mPos <- getCursorPosition fg
           case mPos of
             Just (DisplayRegion w h) -> do
                                  show_cursor $ terminal vty
                                  set_cursor_pos (terminal vty) w h
             Nothing -> hide_cursor $ terminal vty
           next_event vty

  case evt of
    (EvKey k mods) -> handleKeyEvent fg k mods >> return ()
    _ -> return ()

  runUi' vty uiWidget
