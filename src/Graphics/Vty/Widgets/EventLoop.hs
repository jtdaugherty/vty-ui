module Graphics.Vty.Widgets.EventLoop
    ( runUi
    )
where

import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Graphics.Vty
import Graphics.Vty.Widgets.Core
    ( Widget
    , renderAndPosition
    , handleKeyEvent
    , FocusGroup
    , getCursorPosition
    )

runUi :: (MonadIO m) =>
         Vty
      -> Widget a
      -> Widget FocusGroup
      -> m ()
runUi vty uiWidget focusGroup = do
  evt <- liftIO $ do
           sz <- display_bounds $ terminal vty
           img <- renderAndPosition uiWidget (DisplayRegion 0 0) sz Nothing
           update vty $ pic_for_image img

           mPos <- getCursorPosition focusGroup
           case mPos of
             Just (DisplayRegion w h) -> do
                                  show_cursor $ terminal vty
                                  set_cursor_pos (terminal vty) w h
             Nothing -> hide_cursor $ terminal vty
           next_event vty

  case evt of
    (EvKey k mods) -> handleKeyEvent focusGroup k mods >> return ()
    _ -> return ()

  runUi vty uiWidget focusGroup
