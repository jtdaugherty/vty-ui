{-# LANGUAGE CPP #-}
-- |This module provides a basic infrastructure for modelling a user
-- interface widget and converting it to Vty's 'Image' type.
module Graphics.Vty.Widgets.Rendering
    ( Widget(..)
    , mkImage

    -- ** Miscellaneous
    , Orientation(..)
    , withWidth
    , withHeight
    )
where

import GHC.Word ( Word )

import Graphics.Vty
    ( DisplayRegion(DisplayRegion)
    , Attr
    , Image
    , Vty(terminal)
    , display_bounds
    )

-- |A simple orientation type.
data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

-- |The type of user interface widgets.  A 'Widget' provides several
-- properties:
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
-- rendering routine takes one parameter: the size of the space in
-- which the widget should be rendered.  The space is important
-- because it provides a maximum size for the widget.  For widgets
-- that consume all available space, the size of the resulting
-- 'Render' will be equal to the supplied size.  For smaller widgets
-- (e.g., a simple string of text), the size of the 'Render' will
-- likely be much smaller than the supplied size.  In any case, any
-- 'Widget' implementation /must/ obey the rule that the resulting
-- 'Render' must not exceed the supplied 'DisplayRegion' in size.  If
-- it does, there's a good chance your interface will be garbled.
--
-- If the widget has child widgets, the supplied size should be
-- subdivided to fit the child widgets as appropriate.  How the space
-- is subdivided may depend on the growth properties of the children
-- or it may be a matter of policy.
data Widget = Widget {
    -- |Render the widget with the given dimensions.  The result
    -- /must/ not be larger than the specified dimensions, but may be
    -- smaller.
    render :: DisplayRegion -> Image

    -- |Will this widget expand to take advantage of available
    -- horizontal space?
    , growHorizontal :: Bool

    -- |Will this widget expand to take advantage of available
    -- vertical space?
    , growVertical :: Bool

    -- |The primary attribute of this widget, used when composing
    -- widgets.  For example, if you want to compose a widget /A/ with
    -- a space-filling widget /B/, you probably want /B/'s text
    -- attributes to be identical to those of /A/.
    , primaryAttribute :: Attr

    -- |Apply the specified attribute to this widget.
    , withAttribute :: Attr -> Widget
    }

-- |Given a 'Widget' and a 'Vty' object, render the widget using the
-- current size of the terminal controlled by Vty. Returns the
-- rendered 'Widget' as an 'Image'.
mkImage :: Vty -> Widget -> IO Image
mkImage vty w = do
  size <- display_bounds $ terminal vty
  return $ render w size

-- |Modify the width component of a 'DisplayRegion'.
withWidth :: DisplayRegion -> Word -> DisplayRegion
withWidth (DisplayRegion _ h) w = DisplayRegion w h

-- |Modify the height component of a 'DisplayRegion'.
withHeight :: DisplayRegion -> Word -> DisplayRegion
withHeight (DisplayRegion w _) h = DisplayRegion w h
