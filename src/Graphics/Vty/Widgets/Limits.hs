{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides wrapper widgets for enforcing an upper bound
-- on the size of child widgets in one or more dimensions in rows or
-- columns, respectively.  This differs from the ''fixed'' widgets in
-- the Fixed module in that Fixed widgets enforce a fixed size
-- regardless of how big or small the child widget is, and add padding
-- to guarantee that the fixed size is honored.
module Graphics.Vty.Widgets.Limits
    ( VLimit
    , HLimit
    , hLimit
    , vLimit
    , boxLimit
    , setVLimit
    , setHLimit
    , addToVLimit
    , addToHLimit
    , getVLimit
    , getHLimit
    )
where

import Control.Monad
import Control.Monad.Trans
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

data HLimit a = (Show a) => HLimit Int (Widget a)

instance Show (HLimit a) where
    show (HLimit i _) = "HLimit { width = " ++ show i ++ ", ... }"

-- |Impose a maximum horizontal size, in columns, on a 'Widget'.
hLimit :: (MonadIO m, Show a) => Int -> Widget a -> m (Widget (HLimit a))
hLimit maxWidth child = do
  wRef <- newWidget $ \w ->
      w { state = HLimit maxWidth child
        , growHorizontal_ = const $ return False
        , growVertical_ = const $ growVertical child
        , render_ = \this s ctx -> do
                   HLimit width ch <- getState this
                   let region = s `withWidth` fromIntegral (min (toEnum width) (region_width s))
                   render ch region ctx

        , setCurrentPosition_ =
            \this pos -> do
              HLimit _ ch <- getState this
              setCurrentPosition ch pos
        }
  wRef `relayKeyEvents` child
  wRef `relayFocusEvents` child
  return wRef

data VLimit a = (Show a) => VLimit Int (Widget a)

instance Show (VLimit a) where
    show (VLimit i _) = "VLimit { height = " ++ show i ++ ", ... }"

-- |Impose a maximum vertical size, in columns, on a 'Widget'.
vLimit :: (MonadIO m, Show a) => Int -> Widget a -> m (Widget (VLimit a))
vLimit maxHeight child = do
  wRef <- newWidget $ \w ->
      w { state = VLimit maxHeight child
        , growHorizontal_ = const $ growHorizontal child
        , growVertical_ = const $ return False

        , render_ = \this s ctx -> do
                   VLimit height ch <- getState this
                   let region = s `withHeight` fromIntegral (min (toEnum height) (region_height s))
                   render ch region ctx

        , setCurrentPosition_ =
            \this pos -> do
              VLimit _ ch <- getState this
              setCurrentPosition ch pos
        }
  wRef `relayKeyEvents` child
  wRef `relayFocusEvents` child
  return wRef

-- |Set the vertical limit of a child widget's size.
setVLimit :: (MonadIO m) => Widget (VLimit a) -> Int -> m ()
setVLimit wRef lim =
    when (lim >= 1) $
         updateWidgetState wRef $ \(VLimit _ ch) -> VLimit lim ch

-- |Set the horizontal limit of a child widget's size.
setHLimit :: (MonadIO m) => Widget (HLimit a) -> Int -> m ()
setHLimit wRef lim =
    when (lim >= 1) $
         updateWidgetState wRef $ \(HLimit _  ch) -> HLimit lim ch

-- |Add to the vertical limit of a child widget's size.
addToVLimit :: (MonadIO m) => Widget (VLimit a) -> Int -> m ()
addToVLimit wRef delta = do
  lim <- getVLimit wRef
  setVLimit wRef $ lim + delta

-- |Add to the horizontal limit of a child widget's size.
addToHLimit :: (MonadIO m) => Widget (HLimit a) -> Int -> m ()
addToHLimit wRef delta = do
  lim <- getHLimit wRef
  setHLimit wRef $ lim + delta

-- |Get the vertical limit of a child widget's size.
getVLimit :: (MonadIO m) => Widget (VLimit a) -> m Int
getVLimit wRef = do
  (VLimit lim _) <- state <~ wRef
  return lim

-- |Get the horizontal limit of a child widget's size.
getHLimit :: (MonadIO m) => Widget (HLimit a) -> m Int
getHLimit wRef = do
  (HLimit lim _) <- state <~ wRef
  return lim

-- |Impose a horizontal and vertical upper bound on the size of a
-- widget.
boxLimit :: (MonadIO m, Show a) =>
            Int -- ^Maximum width in columns
         -> Int -- ^Maximum height in rows
         -> Widget a
         -> m (Widget (VLimit (HLimit a)))
boxLimit maxWidth maxHeight w = do
  ch <- hLimit maxWidth w
  vLimit maxHeight ch