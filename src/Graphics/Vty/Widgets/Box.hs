module Graphics.Vty.Widgets.Box
    ( Box
    , (<-->)
    , (<++>)
    , hBox
    , vBox
    , setBoxSpacing
    , withBoxSpacing
    )
where

import GHC.Word ( Word )
import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , RenderContext
    , getNormalAttr
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
    , Image
    , char_fill
    , region_width
    , region_height
    , image_width
    , image_height
    , vert_cat
    , horiz_cat
    , empty_image
    )

-- |A simple orientation type.
data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

data Box a b = Box Orientation Int (Widget a) (Widget b)

instance Show (Box a b) where
    show (Box sp orientation _ _) = concat [ "Box { spacing = ", show sp
                                           , ", orientation = ", show orientation
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
box o spacing a b = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Box o spacing a b
        , getGrowHorizontal = \(Box _ _ ch1 ch2) -> do
            h1 <- growHorizontal ch1
            h2 <- growHorizontal ch2
            return $ h1 || h2

        , getGrowVertical = \(Box _ _ ch1 ch2) -> do
            v1 <- growVertical ch1
            v2 <- growVertical ch2
            return $ v1 || v2

        , keyEventHandler =
            \this key mods -> do
              Box _ _ ch1 ch2 <- getState this
              handled <- handleKeyEvent ch1 key mods
              if handled then return True else
                  handleKeyEvent ch2 key mods

        , draw = \this s ctx -> do
                   st@(Box orientation _ _ _) <- getState this

                   case orientation of
                     Vertical ->
                         renderBox s ctx st growVertical growVertical region_height
                                   image_height withHeight
                     Horizontal ->
                         renderBox s ctx st growHorizontal growHorizontal region_width
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

withBoxSpacing :: (MonadIO m) => Int -> Widget (Box a b) -> m (Widget (Box a b))
withBoxSpacing spacing wRef = do
  setBoxSpacing wRef spacing
  return wRef

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: (Show a, Show b) =>
             DisplayRegion
          -> RenderContext
          -> Box a b
          -> (Widget a -> IO Bool) -- growth comparison function
          -> (Widget b -> IO Bool) -- growth comparison function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Image -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> IO Image
renderBox s ctx this growFirst growSecond regDimension renderDimension withDim = do
  let Box orientation spacing first second = this
      actualSpace = s `withDim` (max (regDimension s - toEnum spacing) 0)

      renderOrdered a b = do
        a_img <- render a actualSpace ctx

        let remaining = regDimension actualSpace - renderDimension a_img
            s' = actualSpace `withDim` remaining

        b_img <- render b s' ctx

        return $ if renderDimension a_img >= regDimension actualSpace
                 then [a_img, empty_image]
                 else [a_img, b_img]

      renderHalves = do
        let half = actualSpace `withDim` div (regDimension actualSpace) 2
            half' = if regDimension actualSpace `mod` 2 == 0
                    then half
                    else half `withDim` (regDimension half + 1)
        first_img <- render first half ctx
        second_img <- render second half' ctx
        return [first_img, second_img]

      cat = case orientation of
              Vertical -> vert_cat
              Horizontal -> horiz_cat

  gf <- growFirst first
  gs <- growSecond second

  [img1, img2] <- case (gf, gs) of
                    (True, True) -> renderHalves
                    (False, _) -> renderOrdered first second
                    (_, False) -> do
                                  images <- renderOrdered second first
                                  return $ reverse images

  let spAttr = getNormalAttr ctx
      spacer = case spacing of
                 0 -> empty_image
                 _ -> case orientation of
                         Horizontal -> let h = max (image_height img1) (image_height img2)
                                       in char_fill spAttr ' ' (toEnum spacing) h
                         Vertical -> let w = max (image_width img1) (image_width img2)
                                     in char_fill spAttr ' ' w (toEnum spacing)

  return $ cat [img1, spacer, img2]
