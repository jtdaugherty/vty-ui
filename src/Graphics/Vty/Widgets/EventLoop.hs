{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.Vty.Widgets.EventLoop
    ( runUi
    , EventLoopError(..)
    )
where

import Data.Typeable
    ( Typeable
    )
import Data.Maybe
    ( isNothing
    )
import Control.Exception
    ( Exception
    , throw
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

data EventLoopError = NoFocusGroup
                      deriving (Show, Typeable)

instance Exception EventLoopError

runUi :: (MonadIO m) => Widget a -> Attr -> m ()
runUi uiWidget normalAttr =
    liftIO $ do
      vty <- mkVty
      runUi' vty uiWidget normalAttr `finally` do
               reserve_display $ terminal vty
               shutdown vty

runUi' :: Vty -> Widget a -> Attr -> IO ()
runUi' vty uiWidget normalAttr = do
  mFg <- getFocusGroup uiWidget
  when (isNothing mFg) $ throw NoFocusGroup

  let Just fg = mFg

  sz <- display_bounds $ terminal vty
  img <- renderAndPosition uiWidget (DisplayRegion 0 0) sz normalAttr Nothing
  update vty $ pic_for_image img

  mPos <- getCursorPosition fg
  case mPos of
    Just (DisplayRegion w h) -> do
                        show_cursor $ terminal vty
                        set_cursor_pos (terminal vty) w h
    Nothing -> hide_cursor $ terminal vty

  evt <- next_event vty

  case evt of
    (EvKey k mods) -> handleKeyEvent fg k mods >> return ()
    _ -> return ()

  runUi' vty uiWidget normalAttr
