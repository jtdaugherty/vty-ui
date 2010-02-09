{-# LANGUAGE ExistentialQuantification #-}
-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets.
module Graphics.Vty.Widgets.Base
    ( (<++>)
    , (<-->)
    , text
    , hBox
    , vBox
    , hFill
    , vFill
    )
where

import GHC.Word ( Word )

import Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , Render
    , renderImg
    , renderMany
    , renderWidth
    , renderHeight
    , Orientation(..)
    , withHeight
    , withWidth
    )
import Graphics.Vty
    ( DisplayRegion
    , Attr
    , string
    , char_fill
    , region_width
    , region_height
    )

-- |A text widget consisting of a string rendered using an
-- attribute. See 'text'.
text :: Attr -> String -> Widget
text att content = Widget {
                     growHorizontal = False
                   , growVertical = False
                   , primaryAttribute = att
                   , withAttribute = flip text content
                   , render = renderText att content
                   }

renderText :: Attr -> String -> DisplayRegion -> Render
renderText att content sz = renderImg img
    where
      img = if region_height sz == 0
            then nullImg
            else string att truncated
      truncated = take (fromEnum $ region_width sz) content
      nullImg = string att ""

vFill :: Attr -> Char -> Widget
vFill att c = Widget {
                growHorizontal = False
              , growVertical = True
              , primaryAttribute = att
              , withAttribute = flip vFill c
              , render = \s -> renderImg $ char_fill att c (region_width s)
                         (region_height s)
              }

hFill :: Attr -> Char -> Int -> Widget
hFill att c h = Widget {
                  growHorizontal = True
                , growVertical = False
                , primaryAttribute = att
                , withAttribute = \att' -> hFill att' c h
                , render = \s -> renderImg $ char_fill att c (region_width s)
                           (toEnum h)
                }

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
box :: Orientation -> Widget -> Widget -> Widget
box o a b = Widget {
              growHorizontal = growHorizontal a || growHorizontal b
            , growVertical = growVertical a || growVertical b
            , withAttribute =
                \att ->
                    box o (withAttribute a att) (withAttribute b att)
            , primaryAttribute = primaryAttribute a
            , render =
                \s -> case o of
                        Vertical ->
                            renderBox s (a, b) o growVertical region_height
                                      renderHeight withHeight
                        Horizontal ->
                            renderBox s (a, b) o growHorizontal region_width
                                      renderWidth withWidth
            }

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: DisplayRegion
          -> (Widget, Widget)
          -> Orientation
          -> (Widget -> Bool) -- growth comparison function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Render -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> Render
renderBox s (first, second) orientation grow regDimension renderDimension withDim =
    renderMany orientation ws
        where
          ws = case (grow first, grow second) of
                 (True, True) -> renderHalves
                 (False, _) -> renderOrdered first second
                 (_, False) -> let [a, b] = renderOrdered second first
                               in [b, a]
          renderHalves = let half = s `withDim` div (regDimension s) 2
                             half' = if regDimension s `mod` 2 == 0
                                     then half
                                     else half `withDim` (regDimension half + 1)
                         in [ render first half
                            , render second half' ]
          renderOrdered a b = let renderedA = render a s
                                  renderedB = render b s'
                                  remaining = regDimension s - renderDimension renderedA
                                  s' = s `withDim` remaining
                              in if renderDimension renderedA >= regDimension s
                                 then [renderedA]
                                 else [renderedA, renderedB]

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: Widget -> Widget -> Widget
hBox = box Horizontal

-- |An alias for 'hBox' intended as sugar to chain widgets
-- horizontally.
(<++>) :: Widget -> Widget -> Widget
(<++>) = hBox

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: Widget -> Widget -> Widget
vBox = box Vertical

-- |An alias for 'vBox' intended as sugar to chain widgets vertically.
(<-->) :: Widget -> Widget -> Widget
(<-->) = vBox
