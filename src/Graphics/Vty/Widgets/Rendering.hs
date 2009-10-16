{-# LANGUAGE ExistentialQuantification #-}
-- |This module provides a basic infrastructure for modelling a user
-- interface widget and rendering it to Vty's 'Image' type.  Widget
-- implementors must provide an instance of 'Widget' for a concrete
-- type.
module Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , mkImage

    -- ** Rendering process
    -- |'Widget's are ultimately converted to Vty 'Image's, but this
    -- library uses an intermediate type, 'Render', to represent the
    -- physical layout of the images.  A 'Render' represents the
    -- various primitive rendering constructs which support vertical
    -- and horizontal concatenation and 'Image' addressing.  Once a
    -- 'Widget' has been rendered (see 'render'), the resulting
    -- 'Render' is then put through a /positioning pass/ in which the
    -- sizes and positions of any addressable image regions are stored
    -- (see 'RenderState').  The result is a single 'Image' suitable
    -- for use with Vty's 'Graphics.Vty.pic_for_image' function.
    , RenderState
    , Render
    , renderImg
    , renderAddr
    , renderMany
    , renderWidth
    , renderHeight

    -- ** Widget addressing
    -- |Some widgets, such as editable widgets, require that their
    -- on-screen representations be known after rendering; this
    -- library supports a notion of /widget addressing/ in which a
    -- 'Widget' is marked as /addressable/ (see 'addressable').
    -- Addressable widgets' position and size information ('Address')
    -- will be recorded in the 'RenderState' during rendering in
    -- 'mkImage'.
    , Address
    , address
    , Addressable
    , addressable
    , addrSize
    , addrPosition
    , addAddress

    -- ** Miscellaneous
    , Orientation(..)
    , withWidth
    , withHeight
    )
where

import GHC.Word ( Word )
import qualified Data.Map as Map
import Control.Monad.State ( State, modify, runState )

import Graphics.Vty
    ( DisplayRegion(DisplayRegion)
    , Attr
    , Image
    , Vty(terminal)
    , display_bounds
    , (<|>)
    , (<->)
    , image_width
    , image_height
    , region_width
    , region_height
    , vert_cat
    , horiz_cat
    )

-- |A simple orientation type.
data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

-- |The class of user interface widget types.  A 'Widget' provides
-- several properties:
--
-- * /Growth properties/ which provide information about how to
--   allocate space to widgets depending on their propensity to
--   consume available space
--
-- * A /primary attribute/ which is the attribute most easily
--   identifiable with the widget's visual presentation
--
-- * An /attribute override/ which allows the widget and its children
--   to be rendered using a single attribute specified by the caller
--
-- * A /rendering routine/ which converts the widget's internal state
--   into a 'Render' value.
--
-- Of primary concern is the rendering routine, 'render'.  The
-- rendering routine takes two parameters: the size of the space in
-- which the widget should be rendered, and the widget itself.  The
-- space is important because it provides a maximum size for the
-- widget.  For widgets that consume all available space, the size of
-- the resulting 'Render' will be equal to the supplied size.  For
-- smaller widgets (e.g., a simple string of text), the size of the
-- 'Render' will likely be much smaller than the supplied size.
--
-- If the widget has child widgets, the supplied size should be
-- subdivided to fit the child widgets as appropriate.  How the space
-- is subdivided may depend on the growth properties of the children
-- or it may be a matter of policy.  In any case, rendered child
-- widgets should be constrained to the appropriate size; see other
-- 'Widget' instances for examples of this.
class Widget w where
    -- |Given a widget, render it with the given dimensions.  The
    -- result should not be larger than the specified dimensions, but
    -- may be smaller.
    render :: DisplayRegion -> w -> Render

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

-- |An intermediate type used in the rendering process.  Widgets are
-- converted into collections of 'Image's and represented with this
-- type, using a few primitive rendering instructions to determine how
-- the rendered images are combined to form a complete terminal window
-- image.  See 'render'.
data Render = Img Image
            | Addressed String Render
            | Many Orientation [Render]

-- |Annotate a widget with a rendering identifier so that its
-- rendering address will be stored by the rendering process.  Once
-- the widget has been rendered, its address will be found in the
-- resulting 'RenderState'.  To retrieve the address of such an
-- identifier, use 'address'.
addressable :: (Widget a) => String
            -- ^The identifier of the widget to be used in the
            -- 'RenderState'.
            -> a
            -- ^The widget whose rendering address ('Address') should
            -- be stored.
            -> Addressable
addressable = Addressable

instance Widget Addressable where
    growHorizontal (Addressable _ w) = growHorizontal w
    growVertical (Addressable _ w) = growVertical w
    primaryAttribute (Addressable _ w) = primaryAttribute w
    withAttribute (Addressable ident w) att = Addressable ident
                                              (withAttribute w att)
    render s (Addressable ident w) = renderAddr ident (render s w)

-- |Create a 'Render' containing a single 'Image'.
renderImg :: Image -> Render
renderImg = Img

-- |Create a 'Render' representing a render together with an
-- identifier.  This type of 'Render' is used with 'addressable' to
-- locate a widget's position and dimensions in the final 'Image'.
renderAddr :: String -- ^The identifier of the widget that this
                     -- 'Render' represents. Should be the same
                     -- identifier that was passed to 'addressable'.
           -> Render -- ^The 'Render' to identify.
           -> Render
renderAddr = Addressed

-- |Create a 'Render' representing a collection of renders which
-- should be combined in the specified 'Orientation'.
renderMany :: Orientation -> [Render] -> Render
renderMany = Many

-- |Compute the width, in columns, of a 'Render'.
renderWidth :: Render -> Word
renderWidth (Img img) = image_width img
renderWidth (Addressed _ w) = renderWidth w
renderWidth (Many Vertical ws) = maximum $ map renderWidth ws
renderWidth (Many Horizontal ws) = sum $ map renderWidth ws

-- |Compute the height, in rows, of a 'Render'.
renderHeight :: Render -> Word
renderHeight (Img img) = image_height img
renderHeight (Addressed _ w) = renderHeight w
renderHeight (Many Vertical ws) = sum $ map renderHeight ws
renderHeight (Many Horizontal ws) = maximum $ map renderHeight ws

-- |Given a starting position (usually @'DisplayRegion' 0 0@) and a
-- 'Render', combine the 'Render''s contents into a single 'Image' and
-- track the positions and sizes of any 'Render's with positioning
-- addresses.  Returns the resulting image and a 'RenderState'
-- containing the 'Address' values of all addressable widgets.
doPositioning :: DisplayRegion -> Render -> State RenderState Image
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
-- widget to induce storage of its address, use 'addressable'.
address :: String -> RenderState -> Maybe Address
address = Map.lookup

-- |Add an address for the specified identifier, position, and 'Image'
-- to the 'RenderState'.
addAddress :: String        -- ^The 'Address' identifier.
           -> DisplayRegion -- ^The position of the image.
           -> Image         -- ^The image whose size should be stored.
           -> State RenderState ()
addAddress ident pos img = do
  let rinfo = Address pos (imageSize img)
  modify (Map.insert ident rinfo)

-- |Compute the size of an 'Image' as a 'DisplayRegion'.
imageSize :: Image -> DisplayRegion
imageSize img = DisplayRegion (image_width img) (image_height img)

-- |Given a 'Widget' and a 'Vty' object, render the widget using the
-- current size of the terminal controlled by Vty. Returns the
-- rendered 'Widget' as an 'Image' along with the 'RenderState'
-- containing the 'Address'es of 'addressable' widgets.
mkImage :: (Widget a) => Vty -> a -> IO (Image, RenderState)
mkImage vty w = do
  size <- display_bounds $ terminal vty
  let upperLeft = DisplayRegion 0 0
      rendered = render size w
  return $ runState (doPositioning upperLeft rendered) (Map.fromList [])

-- |Modify the width component of a 'DisplayRegion'.
withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

-- |Modify the height component of a 'DisplayRegion'.
withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h
