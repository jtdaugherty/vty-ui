-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets.
module Graphics.Vty.Widgets.Base
    ( Box
    , hBox
    , vBox
    , (<-->)
    , (<++>)

    , VFill
    , HFill
    , hFill
    , vFill

    , VLimit
    , HLimit
    , hLimit
    , vLimit
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
import Graphics.Vty.Widgets.Rendering
    ( Widget
    , WidgetImpl(..)
    , Orientation(..)
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
    )

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

data Box a b = Box Orientation (Widget a) (Widget b)

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
box :: (MonadIO m) => Orientation -> Widget a -> Widget b -> m (Widget (Box a b))
box o a b = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Box o a b
        , getGrowHorizontal = do
            Box _ ch1 ch2 <- ask
            h1 <- growHorizontal ch1
            h2 <- growHorizontal ch2
            return $ h1 || h2

        , getGrowVertical = do
            Box _ ch1 ch2 <- ask
            v1 <- growVertical ch1
            v2 <- growVertical ch2
            return $ v1 || v2

        , keyEventHandler =
            \this key -> do
              Box _ ch1 ch2 <- getState this
              handled <- handleKeyEvent ch1 key
              if handled then return True else
                  handleKeyEvent ch2 key

        , draw = \this s mAttr -> do
                   st@(Box orientation _ _) <- getState this

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
              Box orientation ch1 ch2 <- getState this
              ch1_size <- getPhysicalSize ch1
              setPhysicalPosition ch1 pos
              case orientation of
                Horizontal -> setPhysicalPosition ch2 $
                              pos `withWidth` ((region_width pos) + (region_width ch1_size))
                Vertical -> setPhysicalPosition ch2 $
                            pos `withHeight` ((region_height pos) + (region_height ch1_size))
        }

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
  let Box orientation first second = this

  let renderOrdered a b = do
        a_img <- render a s mAttr

        let remaining = regDimension s - renderDimension a_img
            s' = s `withDim` remaining

        b_img <- render b s' mAttr

        return $ if renderDimension a_img >= regDimension s
                 then [a_img]
                 else [a_img, b_img]

      renderHalves = do
        let half = s `withDim` div (regDimension s) 2
            half' = if regDimension s `mod` 2 == 0
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

  imgs <- case (gf, gs) of
            (True, True) -> renderHalves
            (False, _) -> renderOrdered first second
            (_, False) -> do
              images <- renderOrdered second first
              return $ reverse images

  return $ cat imgs

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: (MonadIO m) => Widget a -> Widget b -> m (Widget (Box a b))
hBox = box Horizontal

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: (MonadIO m) => Widget a -> Widget b -> m (Widget (Box a b))
vBox = box Vertical

data HLimit a = HLimit Int (Widget a)

-- |Impose a maximum horizontal size, in columns, on a 'Widget'.
hLimit :: (MonadIO m) => Int -> Widget a -> m (Widget (HLimit a))
hLimit maxWidth child = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = HLimit maxWidth child
        , getGrowHorizontal = return False

        , getGrowVertical = do
            HLimit _ ch <- ask
            liftIO $ growVertical ch

        , keyEventHandler =
            \this key -> do
              HLimit _ ch <- getState this
              handleKeyEvent ch key

        , draw = \this s mAttr -> do
                   HLimit width ch <- getState this
                   img <- if region_width s < fromIntegral width
                          then render ch s mAttr
                          else render ch (s `withWidth` fromIntegral width) mAttr
                   return img

        , setPosition =
            \this pos -> do
              (setPosition w) this pos
              HLimit _ ch <- getState this
              setPhysicalPosition ch pos
        }

data VLimit a = VLimit Int (Widget a)

-- |Impose a maximum horizontal size, in columns, on a 'Widget'.
vLimit :: (MonadIO m) => Int -> Widget a -> m (Widget (VLimit a))
vLimit maxHeight child = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = VLimit maxHeight child
        , getGrowVertical = return False
        , getGrowHorizontal = do
            VLimit _ ch <- ask
            liftIO $ growHorizontal ch

        , keyEventHandler =
            \this key -> do
              VLimit _ ch <- getState this
              handleKeyEvent ch key

        , draw = \this s mAttr -> do
                   VLimit height ch <- getState this
                   img <- if region_height s < fromIntegral height
                          then render ch s mAttr
                          else render ch (s `withHeight` fromIntegral height) mAttr
                   return img

        , setPosition =
            \this pos -> do
              (setPosition w) this pos
              VLimit _ ch <- getState this
              setPhysicalPosition ch pos
        }

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
