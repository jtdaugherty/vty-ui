-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets.
module Graphics.Vty.Widgets.Base
    ( Box
    , hBox
    , vBox
    , (<-->)
    , (<++>)
    , setBoxSpacing

    , VFill
    , HFill
    , hFill
    , vFill

    , HCentered
    , hCentered

    , VCentered
    , vCentered

    , centered
    )
where

import GHC.Word ( Word )
import Control.Monad.Trans
    ( MonadIO
    , liftIO
    )
import Control.Monad.Reader
    ( ask
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , newWidget
    , updateWidget
    , render
    , withHeight
    , withWidth
    , growHorizontal
    , growVertical
    , handleKeyEvent
    , getState
    , setPhysicalPosition
    , getPhysicalSize
    , updateWidgetState
    )
import Graphics.Vty
    ( DisplayRegion
    , Attr
    , Image
    , char_fill
    , region_width
    , region_height
    , image_width
    , image_height
    , vert_cat
    , horiz_cat
    , def_attr
    , empty_image
    )

-- |A simple orientation type.
data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

data HCentered a = HCentered (Widget a)

hCentered :: (MonadIO m) => Widget a -> m (Widget (HCentered a))
hCentered ch = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HCentered ch
        , getGrowHorizontal = return True

        , getGrowVertical = do
            HCentered child <- ask
            growVertical child

        , draw = \this s mAttr -> do
                   HCentered child <- getState this
                   img <- render child s mAttr

                   -- XXX def_attr can be wrong
                   let attr' = maybe def_attr id mAttr
                       (half, half') = centered_halves region_width s (image_width img)

                   return $ if half > 0
                            then horiz_cat [ char_fill attr' ' ' half (image_height img)
                                           , img
                                           , char_fill attr' ' ' half' (image_height img)
                                           ]
                            else img

        , setPosition =
            \this pos -> do
              HCentered child <- getState this
              s <- getPhysicalSize this
              chSz <- getPhysicalSize child
              let (half, _) = centered_halves region_width s (region_width chSz)
                  chPos = pos `withWidth` (region_width pos + half)
              setPhysicalPosition child chPos
        }
  return wRef

data VCentered a = VCentered (Widget a)

vCentered :: (MonadIO m) => Widget a -> m (Widget (VCentered a))
vCentered ch = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VCentered ch
        , getGrowVertical = return True

        , getGrowHorizontal = do
            VCentered child <- ask
            growHorizontal child

        , draw = \this s mAttr -> do
                   VCentered child <- getState this
                   img <- render child s mAttr

                   -- XXX def_attr can be wrong
                   let attr' = maybe def_attr id mAttr
                       (half, half') = centered_halves region_height s (image_height img)

                   return $ if half > 0
                            then vert_cat [ char_fill attr' ' ' (image_width img) half
                                          , img
                                          , char_fill attr' ' ' (image_width img) half'
                                          ]
                            else img

        , setPosition =
            \this pos -> do
              VCentered child <- getState this
              s <- getPhysicalSize this
              chSz <- getPhysicalSize child
              let (half, _) = centered_halves region_height s (region_height chSz)
                  chPos = pos `withHeight` (region_height pos + half)
              setPhysicalPosition child chPos
        }
  return wRef

centered :: (MonadIO m) => Widget a -> m (Widget (VCentered (HCentered a)))
centered wRef = vCentered =<< hCentered wRef

centered_halves :: (DisplayRegion -> Word) -> DisplayRegion -> Word -> (Word, Word)
centered_halves region_size s obj_sz =
    let remaining = region_size s - obj_sz
        half = remaining `div` 2
        half' = if remaining `mod` 2 == 0
                then half
                else half + 1
    in (half, half')

data VFill = VFill Attr Char

-- |A vertical fill widget.  Fills all available space with the
-- specified character and attribute.
vFill :: (MonadIO m) => Attr -> Char -> m (Widget VFill)
vFill att c = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VFill att c
        , getGrowHorizontal = return False
        , getGrowVertical = return True
        , draw = \this s mAttr -> do
                   VFill attr ch <- getState this
                   let attr' = maybe attr id mAttr
                   return $ char_fill attr' ch (region_width s) (region_height s)
        }
  return wRef

data HFill = HFill Attr Char Int

-- |A horizontal fill widget.  Fills the available horizontal space,
-- one row high, using the specified character and attribute.
hFill :: (MonadIO m) => Attr -> Char -> Int -> m (Widget HFill)
hFill att c h = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HFill att c h
        , getGrowHorizontal = return True
        , getGrowVertical = return False
        , draw = \this s mAttr -> do
                   HFill attr ch height <- getState this
                   let attr' = maybe attr id mAttr
                   return $ char_fill attr' ch (region_width s) (toEnum height)
        }
  return wRef

data Box a b = Box Orientation Int (Widget a) (Widget b)

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
box :: (MonadIO m) => Orientation -> Int -> Widget a -> Widget b -> m (Widget (Box a b))
box o spacing a b = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Box o spacing a b
        , getGrowHorizontal = do
            Box _ _ ch1 ch2 <- ask
            h1 <- growHorizontal ch1
            h2 <- growHorizontal ch2
            return $ h1 || h2

        , getGrowVertical = do
            Box _ _ ch1 ch2 <- ask
            v1 <- growVertical ch1
            v2 <- growVertical ch2
            return $ v1 || v2

        , keyEventHandler =
            \this key mods -> do
              Box _ _ ch1 ch2 <- getState this
              handled <- handleKeyEvent ch1 key mods
              if handled then return True else
                  handleKeyEvent ch2 key mods

        , draw = \this s mAttr -> do
                   st@(Box orientation _ _ _) <- getState this

                   case orientation of
                     Vertical ->
                         renderBox s mAttr st growVertical growVertical region_height
                                   image_height withHeight
                     Horizontal ->
                         renderBox s mAttr st growHorizontal growHorizontal region_width
                                   image_width withWidth

        , setPosition =
            \this pos -> do
              (setPosition w) this pos
              Box orientation sp ch1 ch2 <- getState this
              ch1_size <- getPhysicalSize ch1
              setPhysicalPosition ch1 pos
              case orientation of
                Horizontal -> setPhysicalPosition ch2 $
                              pos `withWidth` ((region_width pos) + (region_width ch1_size) +
                                                                  toEnum sp)
                Vertical -> setPhysicalPosition ch2 $
                            pos `withHeight` ((region_height pos) + (region_height ch1_size) +
                                                                  toEnum sp)
        }
  return wRef

setBoxSpacing :: (MonadIO m) => Widget (Box a b) -> Int -> m ()
setBoxSpacing wRef spacing =
    updateWidgetState wRef $ \(Box o _ a b) -> Box o spacing a b

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: DisplayRegion
          -> Maybe Attr
          -> Box a b
          -> (Widget a -> IO Bool) -- growth comparison function
          -> (Widget b -> IO Bool) -- growth comparison function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Image -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> IO Image
renderBox s mAttr this growFirst growSecond regDimension renderDimension withDim = do
  let Box orientation spacing first second = this
      actualSpace = s `withDim` (max (regDimension s - toEnum spacing) 0)

      renderOrdered a b = do
        a_img <- render a actualSpace mAttr

        let remaining = regDimension actualSpace - renderDimension a_img
            s' = actualSpace `withDim` remaining

        b_img <- render b s' mAttr

        return $ if renderDimension a_img >= regDimension actualSpace
                 then [a_img, empty_image]
                 else [a_img, b_img]

      renderHalves = do
        let half = actualSpace `withDim` div (regDimension actualSpace) 2
            half' = if regDimension actualSpace `mod` 2 == 0
                    then half
                    else half `withDim` (regDimension half + 1)
        first_img <- render first half mAttr
        second_img <- render second half' mAttr
        return [first_img, second_img]

      cat = case orientation of
              Vertical -> vert_cat
              Horizontal -> horiz_cat

  gf <- liftIO $ growFirst first
  gs <- liftIO $ growSecond second

  [img1, img2] <- case (gf, gs) of
                    (True, True) -> renderHalves
                    (False, _) -> renderOrdered first second
                    (_, False) -> do
                                  images <- renderOrdered second first
                                  return $ reverse images

  let spacer = case spacing of
                 0 -> empty_image
                 _ -> case orientation of
                         Horizontal -> let h = max (image_height img1) (image_height img2)
                                       in char_fill def_attr ' ' (toEnum spacing) h
                         Vertical -> let w = max (image_width img1) (image_width img2)
                                     in char_fill def_attr ' ' w (toEnum spacing)

  return $ cat [img1, spacer, img2]

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: (MonadIO m) => Widget a -> Widget b -> m (Widget (Box a b))
hBox = box Horizontal 0

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: (MonadIO m) => Widget a -> Widget b -> m (Widget (Box a b))
vBox = box Vertical 0

(<-->) :: (MonadIO m) => m (Widget a) -> m (Widget b) -> m (Widget (Box a b))
(<-->) act1 act2 = do
  ch1 <- act1
  ch2 <- act2
  vBox ch1 ch2

(<++>) :: (MonadIO m) => m (Widget a) -> m (Widget b) -> m (Widget (Box a b))
(<++>) act1 act2 = do
  ch1 <- act1
  ch2 <- act2
  hBox ch1 ch2

infixl 2 <-->
infixl 2 <++>
