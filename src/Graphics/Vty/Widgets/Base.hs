{-# LANGUAGE ExistentialQuantification #-}
-- |A collection of primitive user interface widgets for composing and
-- laying out 'Graphics.Vty' user interfaces.  This module provides
-- basic static and box layout widgets and a type class for rendering
-- widgets to Vty 'Graphics.Vty.Image's.
--
-- Each widget type supplied by this library is exported as a type and
-- an associated constructor function (e.g., 'Text' and 'text', 'Box'
-- and 'vBox' / 'hBox').
module Graphics.Vty.Widgets.Base
    ( Widget(..)
    , mkImage
    , AnyWidget
    , Orientation(..)
    , Rendered
    , Address
    , Text
    , Box
    , Fill
    , RenderState
    , Addressable
    , (<++>)
    , (<-->)
    , anyWidget
    , addrSize
    , addrPosition
    , addressable
    , address
    , renderedImg
    , renderedAddr
    , renderedMany
    , renderedWidth
    , renderedHeight
    , text
    , hBox
    , vBox
    , hFill
    , vFill
    )
where

import GHC.Word ( Word )
import qualified Data.Map as Map
import Control.Monad.State ( State, modify, runState )

import Graphics.Vty ( DisplayRegion(DisplayRegion), Vty, Image, Attr
                    , string, char_fill, image_width, image_height
                    , region_width, region_height, terminal
                    , display_bounds, vert_cat, horiz_cat, (<->), (<|>) )

-- |Information about the rendered state of a widget.
data Address = Address { addrPosition :: DisplayRegion
                       -- ^The rendered position of a widget.
                       , addrSize :: DisplayRegion
                       -- ^The rendered size of a widget.
                       }
               deriving (Eq, Show)

-- |The collection of widget names (see 'addressable') and their
-- rendering addresses as a result of 'render'.
type RenderState = Map.Map String Address

data Rendered = Img Image
              | Addressed String Rendered
              | Many Orientation [Rendered]

renderedImg :: Image -> Rendered
renderedImg = Img

renderedAddr :: String -> Rendered -> Rendered
renderedAddr = Addressed

renderedMany :: Orientation -> [Rendered] -> Rendered
renderedMany = Many

renderedWidth :: Rendered -> Word
renderedWidth (Img img) = image_width img
renderedWidth (Addressed _ w) = renderedWidth w
renderedWidth (Many Vertical ws) = maximum $ map renderedWidth ws
renderedWidth (Many Horizontal ws) = sum $ map renderedWidth ws

renderedHeight :: Rendered -> Word
renderedHeight (Img img) = image_height img
renderedHeight (Addressed _ w) = renderedHeight w
renderedHeight (Many Vertical ws) = sum $ map renderedHeight ws
renderedHeight (Many Horizontal ws) = maximum $ map renderedHeight ws

-- |The class of user interface widgets.  Note that the growth
-- properties 'growHorizontal' and 'growVertical' are used to control
-- rendering order; if a widget /can/ grow to fill available space,
-- then neighboring fixed-size widgets will be rendered first so
-- remaining space can be computed.  Then, variable-sized (growable)
-- widgets will be rendered last to consume that space.
class Widget w where
    -- |Given a widget, render it with the given dimensions.  The
    -- result should not be larger than the specified dimensions, but
    -- may be smaller.
    render :: DisplayRegion -> w -> Rendered

    -- |Will this widget expand to take advantage of available
    -- horizontal space?
    growHorizontal :: w -> Bool

    -- |Will this widget expand to take advantage of available
    -- vertical space?
    growVertical :: w -> Bool

    -- |The primary attribute of this widget, used when composing
    -- widgets.  For example, if you want to compose a widget /A/ with
    -- a space-filling widget /B/, you probably want /B/'s text
    -- attributes to be identical to those of /A/.
    primaryAttribute :: w -> Attr

    -- |Apply the specified attribute to this widget.
    withAttribute :: w -> Attr -> w

-- |The type of widgets whose rendering addresses should be stored by
-- the rendering process.  See 'addressable'.  The motivation for this
-- is the need to be able to locate a widget on the screen once the
-- layout algorithms have determined the widget's location and size.
data Addressable = forall w. (Widget w) => Addressable String w

-- |Annotate a widget with a rendering identifier so that its
-- rendering address will be stored by the rendering process.  Once
-- the widget has been rendered, its address will be found in the
-- resulting 'RenderState'; see 'address'.
addressable :: (Widget a) => String
            -- ^The identifier of the widget to be used in the
            -- 'RenderState'.
            -> a
            -- ^The widget whose rendering address ('Address') should
            -- be stored.
            -> Addressable
addressable = Addressable

-- |XXX
doPositioning :: DisplayRegion -> Rendered -> State RenderState Image
doPositioning _ (Img img) = return img
doPositioning _ (Many Vertical []) = error "got empty rendered list"
doPositioning _ (Many Horizontal []) = error "got empty rendered list"

doPositioning pos (Many Vertical widgets) = do
  let positionNext _ [] = return $ vert_cat []
      positionNext p (w:ws) = do
        img <- doPositioning p w
        let newPos = p `withHeight` (region_height p + image_height img)
        n <- positionNext newPos ws
        return (img <-> n)

  positionNext pos widgets

doPositioning pos (Many Horizontal widgets) = do
  let positionNext _ [] = return $ horiz_cat []
      positionNext p (w:ws) = do
        img <- doPositioning p w
        let newPos = p `withWidth` (region_width p + image_width img)
        n <- positionNext newPos ws
        return (img <|> n)

  positionNext pos widgets

doPositioning pos (Addressed s w) = do
  img <- doPositioning pos w
  addAddress s pos img
  return img

-- |Retrieve the rendering address for a given widget.  To annotate a
-- widget to induce storage of its address, see 'addressable'.
address :: String -> RenderState -> Maybe Address
address = Map.lookup

addAddress :: String -> DisplayRegion -> Image -> State RenderState ()
addAddress ident pos img = do
  let rinfo = Address pos (imageSize img)
  modify (Map.insert ident rinfo)

imageSize :: Image -> DisplayRegion
imageSize img = DisplayRegion (image_width img) (image_height img)

instance Widget Addressable where
    growHorizontal (Addressable _ w) = growHorizontal w
    growVertical (Addressable _ w) = growVertical w
    primaryAttribute (Addressable _ w) = primaryAttribute w
    withAttribute (Addressable ident w) att = Addressable ident (withAttribute w att)
    render s (Addressable ident w) = Addressed ident (render s w)

-- |A wrapper for all widget types used in normalizing heterogeneous
-- lists of widgets.  See 'anyWidget'.
data AnyWidget = forall a. (Widget a) => AnyWidget a

instance Widget AnyWidget where
    growHorizontal (AnyWidget w) = growHorizontal w
    growVertical (AnyWidget w) = growVertical w
    render s (AnyWidget w) = render s w
    primaryAttribute (AnyWidget w) = primaryAttribute w
    withAttribute (AnyWidget w) att = AnyWidget (withAttribute w att)

-- |A text widget consisting of a string rendered using an
-- attribute. See 'text'.
data Text = Text Attr String

instance Widget Text where
    growHorizontal _ = False
    growVertical _ = False
    render _ (Text att content) = Img $ string att content
    primaryAttribute (Text att _) = att
    withAttribute (Text _ content) att = Text att content

-- |A fill widget for filling available vertical or horizontal space
-- in a box layout.  See 'vFill' and 'hFill'.
data Fill = VFill Attr Char
          | HFill Attr Char Int

instance Widget Fill where
    growHorizontal (HFill _ _ _) = True
    growHorizontal (VFill _ _) = False

    growVertical (VFill _ _) = True
    growVertical (HFill _ _ _) = False

    primaryAttribute (HFill att _ _) = att
    primaryAttribute (VFill att _) = att

    withAttribute (HFill _ c h) att = HFill att c h
    withAttribute (VFill _ c) att = VFill att c

    render s (VFill att c) = Img $ char_fill att c (region_width s) (region_height s)
    render s (HFill att c h) = Img $ char_fill att c (region_width s) (toEnum h)

data Orientation = Horizontal | Vertical

-- |A box layout widget capable of containing two 'Widget's
-- horizontally or vertically.  See 'hBox' and 'vBox'.  Boxes lay out
-- their children as follows:
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
data Box = forall a b. (Widget a, Widget b) => Box Orientation a b

instance Widget Box where
    growHorizontal (Box _ a b) =
        growHorizontal a || growHorizontal b

    growVertical (Box _ a b) =
        growVertical a || growVertical b

    withAttribute (Box o top bottom) att = Box o
                                           (withAttribute top att)
                                           (withAttribute bottom att)

    -- Not the best way to choose this, but it seems like anything
    -- here is going to be arbitrary.
    primaryAttribute (Box _ top _) = primaryAttribute top

    render s (Box Vertical top bottom) =
        renderBox s (top, bottom) Vertical growVertical region_height renderedHeight withHeight
    render s (Box Horizontal left right) =
        renderBox s (left, right) Horizontal growHorizontal region_width renderedWidth withWidth

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: (Widget a, Widget b) =>
             DisplayRegion
          -> (a, b)
          -> Orientation
          -> (AnyWidget -> Bool) -- growth comparison function
          -> (DisplayRegion -> Word) -- region dimension fetch function
          -> (Rendered -> Word) -- image dimension fetch function
          -> (DisplayRegion -> Word -> DisplayRegion) -- dimension modification function
          -> Rendered
renderBox s (first, second) orientation grow regDimension renderedDimension withDim =
    Many orientation ws
        where
          ws = case (grow $ anyWidget first, grow $ anyWidget second) of
                 (True, True) -> renderHalves
                 (False, _) -> renderOrdered first second
                 (_, False) -> let [a, b] = renderOrdered second first
                               in [b, a]
          renderHalves = let half = s `withDim` div (regDimension s) 2
                             half' = if regDimension s `mod` 2 == 0
                                     then half
                                     else half `withDim` (regDimension half + 1)
                         in [ render half first
                            , render half' second ]
          renderOrdered a b = let renderedA = render s a
                                  renderedB = render s' b
                                  remaining = regDimension s - renderedDimension renderedA
                                  s' = s `withDim` remaining
                              in if renderedDimension renderedA >= regDimension s
                                 then [renderedA]
                                 else [renderedA, renderedB]

withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h

-- |Given a 'Widget' and a 'Vty' object, render the widget using the
-- current size of the terminal controlled by Vty. Returns the
-- rendered 'Widget' as an 'Image'.
mkImage :: (Widget a) => Vty -> a -> IO (Image, RenderState)
mkImage vty w = do
  size <- display_bounds $ terminal vty
  let upperLeft = DisplayRegion 0 0
      rendered = render size w
  return $ runState (doPositioning upperLeft rendered) (Map.fromList [])

-- |Wrap a 'Widget' in the 'AnyWidget' type for normalization
-- purposes.
anyWidget :: (Widget a) => a -> AnyWidget
anyWidget = AnyWidget

-- |Create a 'Text' widget.
text :: Attr -- ^The attribute to use to render the text
     -> String -- ^The text to display
     -> Text
text = Text

-- |Create an horizonal fill widget.
hFill :: Attr -- ^The attribute to use to render the fill
      -> Char -- ^The character to fill
      -> Int -- ^The height, in rows, of the filled area; width of the
             -- fill depends on available space
      -> Fill
hFill = HFill

-- |Create a vertical fill widget.  The dimensions of the widget will
-- depend on available space.
vFill :: Attr -- ^The attribute to use to render the fill
      -> Char -- ^The character to fill
      -> Fill
vFill = VFill

-- |Create a horizontal box layout widget containing two widgets side
-- by side.  Space consumed by the box will depend on its contents and
-- the available space.
hBox :: (Widget a, Widget b) => a -- ^The left widget
     -> b -- ^The right widget
     -> Box
hBox = Box Horizontal

-- |An alias for 'hBox' intended as sugar to chain widgets
-- horizontally.
(<++>) :: (Widget a, Widget b) => a -> b -> Box
(<++>) = hBox

-- |Create a vertical box layout widget containing two widgets.  Space
-- consumed by the box will depend on its contents and the available
-- space.
vBox :: (Widget a, Widget b) => a -- ^The top widget
     -> b -- ^The bottom widget
     -> Box
vBox = Box Vertical

-- |An alias for 'vBox' intended as sugar to chain widgets vertically.
(<-->) :: (Widget a, Widget b) => a -> b -> Box
(<-->) = vBox
