{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.Vty.Widgets.Box
    ( Box
    , ChildSizePolicy(..)
    , IndividualPolicy(..)
    , BoxError(..)
    , (<-->)
    , (<++>)
    , hBox
    , vBox
    , setBoxSpacing
    , withBoxSpacing
    , defaultChildSizePolicy
    , setBoxChildSizePolicy
    , getBoxChildSizePolicy
    , getFirstChild
    , getSecondChild
    )
where

import GHC.Word ( Word )
import Data.Typeable
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Graphics.Vty.Widgets.Core
import Graphics.Vty
import Graphics.Vty.Widgets.Util

data BoxError = BadPercentage
                deriving (Eq, Show, Typeable)

instance Exception BoxError

-- |A simple orientation type.
data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

data IndividualPolicy = BoxAuto
                      | BoxFixed Int
                        deriving (Show, Eq)

data ChildSizePolicy = PerChild IndividualPolicy IndividualPolicy
                     | Percentage Int
                       -- ^percent of space given to first child,
                       -- which implies that given to the second.
                       deriving (Show, Eq)

data Box a b = Box { boxChildSizePolicy :: ChildSizePolicy
                   , boxOrientation :: Orientation
                   , boxSpacing :: Int
                   , boxFirst :: Widget a
                   , boxSecond :: Widget b

                   -- Box layout functions

                   -- growth comparison function
                   , firstGrows :: IO Bool
                   -- growth comparison function
                   , secondGrows :: IO Bool
                   -- region dimension fetch function
                   , regDimension :: DisplayRegion -> Word
                   -- image dimension fetch function
                   , imgDimension :: Image -> Word
                   -- dimension modification function
                   , withDimension :: DisplayRegion -> Word -> DisplayRegion
                   -- Oriented image concatenation
                   , img_cat :: [Image] -> Image
                   }

instance Show (Box a b) where
    show b = concat [ "Box { spacing = ", show $ boxSpacing b
                    , ", childSizePolicy = ", show $ boxChildSizePolicy b
                    , ", orientation = ", show $ boxOrientation b
                    , " }"
                    ]

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: (MonadIO m, Show a, Show b) => Widget a -> Widget b -> m (Widget (Box a b))
hBox = box Horizontal 0

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: (MonadIO m, Show a, Show b) => Widget a -> Widget b -> m (Widget (Box a b))
vBox = box Vertical 0

(<-->) :: (MonadIO m, Show a, Show b) => m (Widget a) -> m (Widget b) -> m (Widget (Box a b))
(<-->) act1 act2 = do
  ch1 <- act1
  ch2 <- act2
  vBox ch1 ch2

(<++>) :: (MonadIO m, Show a, Show b) => m (Widget a) -> m (Widget b) -> m (Widget (Box a b))
(<++>) act1 act2 = do
  ch1 <- act1
  ch2 <- act2
  hBox ch1 ch2

infixl 3 <-->
infixl 3 <++>

defaultChildSizePolicy :: ChildSizePolicy
defaultChildSizePolicy = PerChild BoxAuto BoxAuto

-- |A box layout widget capable of containing two 'Widget's
-- horizontally or vertically.  See 'hBox' and 'vBox'.  Boxes lay out
-- their children by using the growth properties of the children:
--
-- * If both children are expandable in the same dimension (i.e., both
--   vertically or both horizontally), the children are each given
--   half of the parent container's available space
--
-- * If one of the children is expandable and the other is static, the
--   static child is rendered first and the remaining space is given
--   to the expandable child
--
-- * Otherwise, both children are rendered in top-to-bottom or
--   left-to-right order and the resulting container uses only as much
--   space as its children combined
box :: (MonadIO m, Show a, Show b) =>
       Orientation -> Int -> Widget a -> Widget b -> m (Widget (Box a b))
box o spacing wa wb = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Box { boxChildSizePolicy = defaultChildSizePolicy
                      , boxOrientation = o
                      , boxSpacing = spacing
                      , boxFirst = wa
                      , boxSecond = wb

                      , firstGrows =
                          (if o == Vertical then growVertical else growHorizontal) wa
                      , secondGrows =
                          (if o == Vertical then growVertical else growHorizontal) wb
                      , regDimension =
                          if o == Vertical then region_height else region_width
                      , imgDimension =
                          if o == Vertical then image_height else image_width
                      , withDimension =
                          if o == Vertical then withHeight else withWidth
                      , img_cat =
                          if o == Vertical then vert_cat else horiz_cat
                      }
        , growHorizontal_ = \b -> do
            h1 <- growHorizontal $ boxFirst b
            h2 <- growHorizontal $ boxSecond b
            return $ h1 || h2

        , growVertical_ = \b -> do
            v1 <- growVertical $ boxFirst b
            v2 <- growVertical $ boxSecond b
            return $ v1 || v2

        , keyEventHandler =
            \this key mods -> do
              b <- getState this
              handled <- handleKeyEvent (boxFirst b) key mods
              if handled then return True else
                  handleKeyEvent (boxSecond b) key mods

        , render_ = \this s ctx -> do
                      b <- getState this
                      renderBox s ctx b

        , setCurrentPosition_ =
            \this pos -> do
              b <- getState this
              ch1_size <- getCurrentSize $ boxFirst b
              setCurrentPosition (boxFirst b) pos
              case boxOrientation b of
                Horizontal -> setCurrentPosition (boxSecond b) $
                              pos `plusWidth` ((region_width ch1_size) + (toEnum $ boxSpacing b))
                Vertical -> setCurrentPosition (boxSecond b) $
                            pos `plusHeight` ((region_height ch1_size) + (toEnum $ boxSpacing b))
        }

  wRef `relayFocusEvents` wa
  wRef `relayFocusEvents` wb

  return wRef

getFirstChild :: (MonadIO m) => Widget (Box a b) -> m (Widget a)
getFirstChild = (boxFirst <~~)

getSecondChild :: (MonadIO m) => Widget (Box a b) -> m (Widget b)
getSecondChild = (boxSecond <~~)

setBoxSpacing :: (MonadIO m) => Widget (Box a b) -> Int -> m ()
setBoxSpacing wRef spacing =
    updateWidgetState wRef $ \b -> b { boxSpacing = spacing }

withBoxSpacing :: (MonadIO m) => Int -> Widget (Box a b) -> m (Widget (Box a b))
withBoxSpacing spacing wRef = do
  setBoxSpacing wRef spacing
  return wRef

getBoxChildSizePolicy :: (MonadIO m) => Widget (Box a b) -> m ChildSizePolicy
getBoxChildSizePolicy = (boxChildSizePolicy <~~)

setBoxChildSizePolicy :: (MonadIO m) => Widget (Box a b) -> ChildSizePolicy -> m ()
setBoxChildSizePolicy b spol = do
  case spol of
    Percentage v -> when (v < 0 || v > 100) $ throw BadPercentage
    _ -> return ()

  updateWidgetState b $ \s -> s { boxChildSizePolicy = spol }

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: (Show a, Show b) =>
             DisplayRegion
          -> RenderContext
          -> Box a b
          -> IO Image
renderBox s ctx this = do
  let actualSpace = regDimension this s - (toEnum (boxSpacing this))

  (img1, img2) <-
      -- XXX fix for case where we don't have enough space to honor
      -- hard-coded sizes (either fixed or derived fixed)

      -- XXX also check for overflow
      case boxChildSizePolicy this of
        PerChild BoxAuto BoxAuto -> renderBoxAuto s ctx this
        Percentage v -> do
                         let firstDim = round (fromRational
                                        (fromRational ((toRational v) / (100.0)) *
                                                          (toRational actualSpace)) ::Rational)
                             secondDim = fromEnum (actualSpace - firstDim)
                         renderBoxFixed s ctx this (fromEnum firstDim) secondDim
        PerChild BoxAuto (BoxFixed v) -> do
                                     let remaining = fromEnum (actualSpace - toEnum v)
                                     renderBoxFixed s ctx this remaining v
        PerChild (BoxFixed v) BoxAuto -> do
                                     let remaining = fromEnum (actualSpace - toEnum v)
                                     renderBoxFixed s ctx this v remaining
        PerChild (BoxFixed v1) (BoxFixed v2) -> renderBoxFixed s ctx this v1 v2

  let spAttr = getNormalAttr ctx
      spacing = boxSpacing this
      spacer = case spacing of
                 0 -> empty_image
                 _ -> case boxOrientation this of
                         Horizontal -> let h = max (image_height img1) (image_height img2)
                                       in char_fill spAttr ' ' (toEnum spacing) h
                         Vertical -> let w = max (image_width img1) (image_width img2)
                                     in char_fill spAttr ' ' w (toEnum spacing)

  return $ (img_cat this) [img1, spacer, img2]

renderBoxFixed :: (Show a, Show b) =>
                  DisplayRegion
               -> RenderContext
               -> Box a b
               -> Int
               -> Int
               -> IO (Image, Image)
renderBoxFixed s ctx this firstDim secondDim = do
  let withDim = withDimension this
  img1 <- render (boxFirst this) (s `withDim` (toEnum firstDim)) ctx
  img2 <- render (boxSecond this) (s `withDim` (toEnum secondDim)) ctx

  -- pad the images so they fill the space appropriately.
  let fill img amt = case boxOrientation this of
                       Vertical -> char_fill (getNormalAttr ctx) ' ' (image_width img) amt
                       Horizontal -> char_fill (getNormalAttr ctx) ' ' amt (image_height img)
      firstDimW = toEnum firstDim
      secondDimW = toEnum secondDim
      img1_size = (imgDimension this) img1
      img2_size = (imgDimension this) img2
      img1_padded = if img1_size < firstDimW
                    then (img_cat this) [img1, fill img1 (firstDimW - img1_size)]
                    else img1
      img2_padded = if img2_size < secondDimW
                    then (img_cat this) [img2, fill img2 (secondDimW - img2_size)]
                    else img2

  return (img1_padded, img2_padded)

renderBoxAuto :: (Show a, Show b) =>
                 DisplayRegion
              -> RenderContext
              -> Box a b
              -> IO (Image, Image)
renderBoxAuto s ctx this = do
  let spacing = boxSpacing this
      first = boxFirst this
      second = boxSecond this
      withDim = withDimension this
      renderDimension = imgDimension this
      regDim = regDimension this

      actualSpace = s `withDim` (max (regDim s - toEnum spacing) 0)

      renderOrdered a b = do
        a_img <- render a actualSpace ctx

        let remaining = regDim actualSpace - renderDimension a_img
            s' = actualSpace `withDim` remaining

        b_img <- render b s' ctx

        return $ if renderDimension a_img >= regDim actualSpace
                 then [a_img, empty_image]
                 else [a_img, b_img]

      renderHalves = do
        let half = actualSpace `withDim` div (regDim actualSpace) 2
            half' = if regDim actualSpace `mod` 2 == 0
                    then half
                    else half `withDim` (regDim half + 1)
        first_img <- render first half ctx
        second_img <- render second half' ctx
        return [first_img, second_img]

  gf <- firstGrows this
  gs <- secondGrows this

  [img1, img2] <- case (gf, gs) of
                    (True, True) -> renderHalves
                    (False, _) -> renderOrdered first second
                    (_, False) -> do
                                  images <- renderOrdered second first
                                  return $ reverse images

  return (img1, img2)
