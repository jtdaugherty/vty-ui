-- |This module provides \"text clipping\" routines.  These routines
-- are responsible for ensuring that logical characters are clipped
-- properly when being laid out in a given physical region.  This is a
-- bit tricky because some Unicode characters use two terminal columns
-- and others (most) use one.  We have to take this into account when
-- truncating text to fit into rendering regions, so we concentrate
-- that logic here under the name of a \"clipping rectangle\" and
-- functions to apply it.
--
-- Clipping functionality is provided in two forms: one- and
-- two-dimensional clipping.  The former is useful for clipping a
-- single line of text at a given offset and up to a given width.  The
-- latter is useful for clipping a list of lines with respect to a 2-D
-- clipping rectangle.
module Graphics.Vty.Widgets.TextClip
    ( ClipRect(..)
    , clip1d
    , clip2d
    , updateRect
    )
where

import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import Graphics.Vty.Widgets.Util
    ( Phys(..)
    , chWidth
    )

-- |The type of clipping rectangles for 2-D clipping operations.  All
-- values are 'Phys' values to indicate that we are dealing explicitly
-- with physical column measurements rather than logical character
-- positions.
data ClipRect =
    ClipRect { clipLeft :: Phys
             -- ^The left margin of the clipping rectangle.
             , clipTop :: Phys
             -- ^The top row of the clipping rectangle.
             , clipWidth :: Phys
             -- ^The width, in columns, of the clipping rectangle.
             , clipHeight :: Phys
             -- ^The height, in rows, of the clipping rectangle.
             }
    deriving (Eq, Show)

-- |One-dimensional text clipping.  Returns the clipped text plus
-- 'Bool's indicating whether wide characters were \"sliced\" on
-- either side (left and right, respectively) of the clipping region.
-- This function guarantees that the text returned will always fit
-- within the specified clipping region.  Since wide characters may be
-- sliced during clipping, this may return a text string smaller than
-- the clipping region.
clip1d :: Phys -> Phys -> T.Text -> (T.Text, Bool, Bool)
clip1d _ 0 _ = (T.empty, False, False)
clip1d start len t = (T.pack result2, lSlice, rSlice)

    where
      pairs = [ (c, chWidth c) | c <- T.unpack t ]

      exploded = concat $ mkExp <$> pairs
      mkExp (a, i) = Just a : replicate (fromEnum i - 1) Nothing

      -- First clip up to the starting position.
      clip1 = drop (fromEnum start) exploded
      -- Then clip according to the width.
      clip2 = take (fromEnum len) clip1
      -- Rest is whatever was left after clipping to the width.
      rest = drop (fromEnum len) clip1

      rSlice = length rest > 0 && head rest == Nothing
      lSlice = length clip1 > 0 && head clip1 == Nothing

      result1 = catMaybes clip2
      result2 = if rSlice
                then init result1
                else result1

-- |Two-dimensional text clipping.  Returns clipping data for each
-- line as returned by 'clip1d', with the added behavior that it
-- returns at most 'clipHeight' lines of text and uses 'clipTop' as
-- the offset when clipping rows.
clip2d :: ClipRect -> [T.Text] -> [(T.Text, Bool, Bool)]
clip2d rect ls = clip1d left len <$> visibleLines
        where
          visibleLines = take (fromEnum height) $ drop (fromEnum top) ls
          left = clipLeft rect
          top = clipTop rect
          len = clipWidth rect
          height = clipHeight rect

-- |Given a physical point and a clipping rectangle, adjust the
-- clipping rectangle so that the point falls just inside the
-- rectangle.  If the point is already within the rectangle, return
-- the rectangle unmodified.  NB: this assumes that the physical
-- position given has passed whatever validation checks are relevant
-- for the user of the 'ClipRect'.  This function just performs a
-- rectangle transformation.
updateRect :: (Phys, Phys) -> ClipRect -> ClipRect
updateRect (row, col) oldRect = adjustLeft $ adjustTop oldRect
    where
      adjustLeft old
          | col < clipLeft oldRect = old { clipLeft = col }
          | col >= clipLeft oldRect + clipWidth oldRect =
              old { clipLeft = col - clipWidth old + 1 }
          | otherwise = old

      adjustTop old
          | row < clipTop oldRect = old { clipTop = row }
          | row >= clipTop oldRect + clipHeight oldRect =
              old { clipTop = row - clipHeight old + 1 }
          | otherwise = old
