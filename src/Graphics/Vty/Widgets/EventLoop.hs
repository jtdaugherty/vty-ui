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
    , RenderContext
    , renderAndPosition
    , handleKeyEvent
    , getFocusGroup
    , getCursorPosition
    )

data EventLoopError = NoFocusGroup
                      deriving (Show, Typeable)

instance Exception EventLoopError

runUi :: (MonadIO m, Show a) => Widget a -> RenderContext -> m ()
runUi uiWidget ctx =
    liftIO $ do
      vty <- mkVty
      runUi' vty uiWidget ctx `finally` do
               reserve_display $ terminal vty
               shutdown vty

runUi' :: (Show a) => Vty -> Widget a -> RenderContext -> IO ()
runUi' vty uiWidget ctx = do
  mFg <- getFocusGroup uiWidget
  when (isNothing mFg) $ throw NoFocusGroup

  let Just fg = mFg

  sz <- display_bounds $ terminal vty
  img <- renderAndPosition uiWidget (DisplayRegion 0 0) sz ctx
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

  runUi' vty uiWidget ctx
