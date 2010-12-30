-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets.
module Graphics.Vty.Widgets.Base
    ( Box
    , (<++>)
    , (<-->)
    , hBox
    , vBox

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
import Control.Monad.Reader
    ( ask
    )
import Control.Monad.State
    ( State
    , get
    , put
    )
import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Orientation(..)
    , render
    , withHeight
    , withWidth
    , primaryAttribute
    , withAttribute
    , growHorizontal
    , growVertical
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
vFill :: Attr -> Char -> Widget VFill
vFill att c = Widget {
                state = VFill att c
              , getGrowHorizontal = return False
              , getGrowVertical = return True

              , getPrimaryAttribute = do
                  VFill attr _ <- ask
                  return attr

              , newWithAttribute = \attr -> do
                  VFill _ ch <- ask
                  return $ vFill attr ch

              , draw = \s -> do
                         VFill attr ch <- get
                         return $ char_fill attr ch (region_width s) (region_height s)
              }

data HFill = HFill Attr Char Int

-- |A horizontal fill widget.  Fills the available horizontal space,
-- one row high, using the specified character and attribute.
hFill :: Attr -> Char -> Int -> Widget HFill
hFill att c h = Widget {
                  state = HFill att c h
                , getGrowHorizontal = return True
                , getGrowVertical = return False

                , getPrimaryAttribute = do
                    HFill attr _ _ <- ask
                    return attr

                , newWithAttribute = \attr -> do
                    HFill _ ch height <- ask
                    return $ hFill attr ch height

                , draw = \s -> do
                           HFill attr ch height <- get
                           return $ char_fill attr ch (region_width s) (toEnum height)
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
box :: Orientation -> Widget a -> Widget b -> Widget (Box a b)
box o a b = Widget {
              state = Box o a b
            , getGrowHorizontal = do
                Box _ ch1 ch2 <- ask
                return $ growHorizontal ch1 || growHorizontal ch2

            , getGrowVertical = do
                Box _ ch1 ch2 <- ask
                return $ growVertical ch1 || growVertical ch2

            , newWithAttribute =
                \attr -> do
                  Box orientation ch1 ch2 <- ask
                  return $ box orientation (withAttribute ch1 attr) (withAttribute ch2 attr)

            , getPrimaryAttribute = do
                Box _ ch1 _ <- ask
                return $ primaryAttribute ch1

            , draw = \s -> do
                       Box orientation _ _ <- get
                       case orientation of
                         Vertical ->
                             renderBox s growVertical growVertical region_height
                                       image_height withHeight
                         Horizontal ->
                             renderBox s growHorizontal growHorizontal region_width
                                       image_width withWidth
            }

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: DisplayRegion
          -> (Widget a -> Bool) -- growth comparison function
          -> (Widget b -> Bool) -- growth comparison function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Image -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> State (Box a b) Image
renderBox s growFirst growSecond regDimension renderDimension withDim = do
  Box orientation first second <- get

  let renderOrdered a b = let (a_img, a') = render a s
                              (b_img, b') = render b s'
                              remaining = regDimension s - renderDimension a_img
                              s' = s `withDim` remaining
                          in if renderDimension a_img >= regDimension s
                             then ([a_img], (a', b))
                             else ([a_img, b_img], (a', b'))
      renderHalves = let half = s `withDim` div (regDimension s) 2
                         half' = if regDimension s `mod` 2 == 0
                                 then half
                                 else half `withDim` (regDimension half + 1)
                         (first_img, first_half) = render first half
                         (second_img, second_half) = render second half'
                     in ([first_img, second_img], (first_half, second_half))
      cat = case orientation of
              Vertical -> vert_cat
              Horizontal -> horiz_cat
      (imgs, (first', second')) = case (growFirst first, growSecond second) of
                     (True, True) -> renderHalves
                     (False, _) -> renderOrdered first second
                     (_, False) -> let (images, (b', a')) = renderOrdered second first
                                   in (reverse images, (a', b'))

  put $ Box orientation first' second'
  return $ cat imgs

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: Widget a -> Widget b -> Widget (Box a b)
hBox = box Horizontal

-- |An alias for 'hBox' intended as sugar to chain widgets
-- horizontally.
(<++>) :: Widget a -> Widget b -> Widget (Box a b)
(<++>) = hBox

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: Widget a -> Widget b -> Widget (Box a b)
vBox = box Vertical

-- |An alias for 'vBox' intended as sugar to chain widgets vertically.
(<-->) :: Widget a -> Widget b -> Widget (Box a b)
(<-->) = vBox

data HLimit a = HLimit Int (Widget a)

-- |Impose a maximum horizontal size, in columns, on a 'Widget'.
hLimit :: Int -> Widget a -> Widget (HLimit a)
hLimit maxWidth w =
    Widget { state = HLimit maxWidth w
           , getGrowHorizontal = return False
           -- XXX! should depend on state, not closure
           , getGrowVertical = do
               HLimit _ child <- ask
               return $ growVertical child

           , getPrimaryAttribute = do
               HLimit _ child <- ask
               return $ primaryAttribute child

           , newWithAttribute =
               \attr -> do
                 HLimit width child <- ask
                 return $ hLimit width $ withAttribute child attr

           , draw = \s -> do
                      HLimit width child <- get
                      let (img, child') = if region_width s < fromIntegral width
                                          then render child s
                                          else render child $ s `withWidth` fromIntegral width
                      put $ HLimit width child'
                      return img
           }

data VLimit a = VLimit Int (Widget a)

-- |Impose a maximum horizontal size, in columns, on a 'Widget'.
vLimit :: Int -> Widget a -> Widget (VLimit a)
vLimit maxHeight w =
    Widget { state = VLimit maxHeight w
           , getGrowVertical = return False
           -- XXX! should depend on state, not closure
           , getGrowHorizontal = do
               VLimit _ child <- ask
               return $ growHorizontal child

           , getPrimaryAttribute = do
               VLimit _ child <- ask
               return $ primaryAttribute child

           , newWithAttribute =
               \attr -> do
                 VLimit height child <- ask
                 return $ vLimit height $ withAttribute child attr

           , draw = \s -> do
                      VLimit height child <- get
                      let (img, child') = if region_height s < fromIntegral height
                                          then render child s
                                          else render child $ s `withHeight` fromIntegral height
                      put $ VLimit height child'
                      return img
           }
