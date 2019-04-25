{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides wrapper widgets for fixing the size of child
-- widgets in one or more dimensions in rows or columns, respectively.
-- This differs from the ''limit'' widgets in the Limits module in
-- that Limits enforce an upper bound on size.
module Graphics.Vty.Widgets.Fixed
    ( VFixed
    , HFixed
    , hFixed
    , vFixed
    , boxFixed
    , setVFixed
    , setHFixed
    , addToVFixed
    , addToHFixed
    , getVFixedSize
    , getHFixedSize
    )
where

import Control.Monad
import Graphics.Vty hiding (regionHeight, regionWidth)
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

data HFixed a = (Show a) => HFixed Int (Widget a)

instance Show (HFixed a) where
    show (HFixed i _) = "HFixed { width = " ++ show i ++ ", ... }"

-- |Impose a fixed horizontal size, in columns, on a 'Widget'.
hFixed :: (Show a) => Int -> Widget a -> IO (Widget (HFixed a))
hFixed fixedWidth child = do
  let initSt = HFixed fixedWidth child
  wRef <- newWidget initSt $ \w ->
      w { render_ = \this s ctx -> do
                   HFixed width ch <- getState this
                   let region = s `withWidth` fromIntegral (min (toEnum width) (regionWidth s))
                   img <- render ch region ctx
                   -- Pad the image if it's smaller than the region.
                   let img' = if imageWidth img < regionWidth region
                              then img <|> (charFill (getNormalAttr ctx) ' '
                                            (toEnum width - imageWidth img)
                                            1)
                              else img
                   return img'

        , setCurrentPosition_ =
            \this pos -> do
              HFixed _ ch <- getState this
              setCurrentPosition ch pos

        , getCursorPosition_ = \this -> do
              HFixed _ ch <- getState this
              getCursorPosition ch
        }
  wRef `relayKeyEvents` child
  wRef `relayFocusEvents` child
  return wRef

data VFixed a = (Show a) => VFixed Int (Widget a)

instance Show (VFixed a) where
    show (VFixed i _) = "VFixed { height = " ++ show i ++ ", ... }"

-- |Impose a fixed vertical size, in columns, on a 'Widget'.
vFixed :: (Show a) => Int -> Widget a -> IO (Widget (VFixed a))
vFixed maxHeight child = do
  let initSt = VFixed maxHeight child
  wRef <- newWidget initSt $ \w ->
      w { growHorizontal_ = const $ growHorizontal child

        , render_ = \this s ctx -> do
                   VFixed height ch <- getState this
                   let region = s `withHeight` fromIntegral (min (toEnum height) (regionHeight s))
                   img <- render ch region ctx
                   -- Pad the image if it's smaller than the region.
                   let img' = if imageHeight img < regionHeight region
                              then img <-> (charFill (getNormalAttr ctx) ' '
                                            1
                                            (toEnum height - imageHeight img))
                              else img
                   return img'

        , setCurrentPosition_ =
            \this pos -> do
              VFixed _ ch <- getState this
              setCurrentPosition ch pos

        , getCursorPosition_ = \this -> do
              VFixed _ ch <- getState this
              getCursorPosition ch
        }
  wRef `relayKeyEvents` child
  wRef `relayFocusEvents` child
  return wRef

-- |Set the vertical fixed size of a child widget.
setVFixed :: Widget (VFixed a) -> Int -> IO ()
setVFixed wRef lim =
    when (lim >= 1) $
         updateWidgetState wRef $ \(VFixed _ ch) -> VFixed lim ch

-- |Set the horizontal fixed size of a child widget.
setHFixed :: Widget (HFixed a) -> Int -> IO ()
setHFixed wRef lim =
    when (lim >= 1) $
         updateWidgetState wRef $ \(HFixed _  ch) -> HFixed lim ch

-- |Add to the vertical fixed size of a child widget.
addToVFixed :: Widget (VFixed a) -> Int -> IO ()
addToVFixed wRef delta = do
  lim <- getVFixedSize wRef
  setVFixed wRef $ lim + delta

-- |Add to the horizontal fixed size of a child widget.
addToHFixed :: Widget (HFixed a) -> Int -> IO ()
addToHFixed wRef delta = do
  lim <- getHFixedSize wRef
  setHFixed wRef $ lim + delta

-- |Get the vertical fixed size of a child widget.
getVFixedSize :: Widget (VFixed a) -> IO Int
getVFixedSize wRef = do
  (VFixed lim _) <- state <~ wRef
  return lim

-- |Get the horizontal fixed size of a child widget.
getHFixedSize :: Widget (HFixed a) -> IO Int
getHFixedSize wRef = do
  (HFixed lim _) <- state <~ wRef
  return lim

-- |Impose a fixed horizontal and vertical size on a widget.
boxFixed :: (Show a) =>
            Int -- ^Width in columns
         -> Int -- ^Height in rows
         -> Widget a
         -> IO (Widget (VFixed (HFixed a)))
boxFixed maxWidth maxHeight w = do
  ch <- hFixed maxWidth w
  vFixed maxHeight ch
