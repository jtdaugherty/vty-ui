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

data ClipRect =
    ClipRect { clipLeft :: Phys
             , clipTop :: Phys
             , clipWidth :: Phys
             , clipHeight :: Phys
             }
    deriving (Eq, Show)

-- First, 1D clipping.  Returns the clipped data, plus bools
-- indicating whether data elements were sliced on either end.  The
-- data element list will be the list of complete elements that fit
-- inside the clipping region; if one or both of the slice indicators
-- is true, the resulting list will smaller than the clipping region.
clip1d :: Phys -> Phys -> T.Text -> (T.Text, Bool, Bool)
clip1d _ 0 _ = (T.empty, False, False)
clip1d start len t = (T.pack result2, lSlice, rSlice)

    where
      pairs = [ (c, chWidth c) | c <- T.unpack t ]

      exploded = concat $ mkExp <$> pairs
      mkExp (a, i) = Just a : replicate (fromEnum i - 1) Nothing

      clip1 = drop (fromEnum start) exploded
      clip2 = take (fromEnum len) clip1
      rest = drop (fromEnum len) clip1

      rSlice = length rest > 0 && head rest == Nothing
      lSlice = length clip1 > 0 && head clip1 == Nothing

      result1 = catMaybes clip2
      result2 = if rSlice
                then init result1
                else result1

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
-- for the user of the ClipRect.  This function just performs a
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
