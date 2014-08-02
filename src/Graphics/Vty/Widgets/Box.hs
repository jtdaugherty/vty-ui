{-# LANGUAGE DeriveDataTypeable #-}
-- |This module provides vertical and horizontal box layout widgets
-- using the 'Box' type.  Box widgets use their child widgets' size
-- policies and their space allocation settings to determine layout.
--
-- Box widgets propagate key and focus events to their children.
--
-- For more details, see the Vty-ui User's Manual.
module Graphics.Vty.Widgets.Box
    ( Box
    , ChildSizePolicy(..)
    , IndividualPolicy(..)
    , BoxError(..)
    -- * Box Constructors
    , hBox
    , vBox
    , (<++>)
    , (<-->)
    -- * Box Configuration
    , setBoxSpacing
    , withBoxSpacing
    , defaultChildSizePolicy
    , setBoxChildSizePolicy
    , getBoxChildSizePolicy
    -- * Child Widget References
    , getFirstChild
    , getSecondChild
    )
where

import Data.Typeable
import Control.Exception
import Control.Monad
import Graphics.Vty.Widgets.Core
import Graphics.Vty
import Graphics.Vty.Widgets.Util

data BoxError = BadPercentage
                -- ^Indicates that a given percentage value was
                -- invalid.
                deriving (Eq, Show, Typeable)

instance Exception BoxError

data Orientation = Horizontal | Vertical
                   deriving (Eq, Show)

-- |Individual child widget policy applied to a child widget contained
-- in a box.
data IndividualPolicy = BoxAuto
                      -- ^The child's growth policy will be used to
                      -- determine layout.  The child widget layout
                      -- will also be affected by the policy of the
                      -- other widget in the box.
                      | BoxFixed Int
                        -- ^A fixed number of rows or columns,
                        -- depending on box type, will be allocated to
                        -- the child.
                        deriving (Show, Eq)

-- |Child size policy applied to a box.
data ChildSizePolicy = PerChild IndividualPolicy IndividualPolicy
                     -- ^A per-child policy.
                     | Percentage Int
                       -- ^Percentage, p, of space given to first
                       -- child, which implies that (100 - p) percent
                       -- given to the second.
                       deriving (Show, Eq)

data Box a b = Box { boxChildSizePolicy :: ChildSizePolicy
                   , boxOrientation :: Orientation
                   , boxSpacing :: Int
                   , boxFirst :: Widget a
                   , boxSecond :: Widget b

                   -- Box layout functions

                   -- growth comparison function
                   , firstGrows :: IO Bool
                   -- growth comparison function
                   , secondGrows :: IO Bool
                   -- region dimension fetch function
                   , regDimension :: DisplayRegion -> Int
                   -- image dimension fetch function
                   , imgDimension :: Image -> Int
                   -- dimension modification function
                   , withDimension :: DisplayRegion -> Int -> DisplayRegion
                   -- Oriented image concatenation
                   , img_cat :: [Image] -> Image
                   }

instance Show (Box a b) where
    show b = concat [ "Box { spacing = ", show $ boxSpacing b
                    , ", childSizePolicy = ", show $ boxChildSizePolicy b
                    , ", orientation = ", show $ boxOrientation b
                    , " }"
                    ]

-- |Create a horizontal box widget containing two widgets side by
-- side.  Space consumed by the box will depend on its contents,
-- available space, and the box child size policy.
hBox :: (Show a, Show b) => Widget a -> Widget b -> IO (Widget (Box a b))
hBox = box Horizontal 0

-- |Create a vertical box widget containing two widgets, one above the
-- other.  Space consumed by the box will depend on its contents,
-- available space, and the box child size policy.
vBox :: (Show a, Show b) => Widget a -> Widget b -> IO (Widget (Box a b))
vBox = box Vertical 0

-- |Create a vertical box widget using monadic widget constructors.
(<-->) :: (Show a, Show b) => IO (Widget a) -> IO (Widget b) -> IO (Widget (Box a b))
(<-->) act1 act2 = do
  ch1 <- act1
  ch2 <- act2
  vBox ch1 ch2

-- |Create a horizontal box widget using monadic widget constructors.
(<++>) :: (Show a, Show b) => IO (Widget a) -> IO (Widget b) -> IO (Widget (Box a b))
(<++>) act1 act2 = do
  ch1 <- act1
  ch2 <- act2
  hBox ch1 ch2

infixl 3 <-->
infixl 3 <++>

-- |The default box child size policy, which defers to the children to
-- determine layout.
defaultChildSizePolicy :: ChildSizePolicy
defaultChildSizePolicy = PerChild BoxAuto BoxAuto

box :: (Show a, Show b) =>
       Orientation -> Int -> Widget a -> Widget b -> IO (Widget (Box a b))
box o spacing wa wb = do
  let initSt = Box { boxChildSizePolicy = defaultChildSizePolicy
                   , boxOrientation = o
                   , boxSpacing = spacing
                   , boxFirst = wa
                   , boxSecond = wb

                   , firstGrows =
                       (if o == Vertical then growVertical else growHorizontal) wa
                   , secondGrows =
                       (if o == Vertical then growVertical else growHorizontal) wb
                   , regDimension =
                       if o == Vertical then snd else fst
                   , imgDimension =
                       if o == Vertical then imageHeight else imageWidth
                   , withDimension =
                       if o == Vertical then withHeight else withWidth
                   , img_cat =
                       if o == Vertical then vertCat else horizCat
                   }

  wRef <- newWidget initSt $ \w ->
      w { growHorizontal_ = \b -> do
            case boxOrientation b of
              Vertical -> do
                h1 <- growHorizontal $ boxFirst b
                h2 <- growHorizontal $ boxSecond b
                return $ h1 || h2
              Horizontal -> do
                case boxChildSizePolicy b of
                  Percentage _ -> return True
                  PerChild s1 s2 -> do
                    h1 <- growHorizontal $ boxFirst b
                    h2 <- growHorizontal $ boxSecond b
                    return $ (h1 && s1 == BoxAuto) || (h2 && s2 == BoxAuto)

        , growVertical_ = \b -> do
            case boxOrientation b of
              Horizontal -> do
                h1 <- growVertical $ boxFirst b
                h2 <- growVertical $ boxSecond b
                return $ h1 || h2
              Vertical -> do
                case boxChildSizePolicy b of
                  Percentage _ -> return True
                  PerChild s1 s2 -> do
                    h1 <- growVertical $ boxFirst b
                    h2 <- growVertical $ boxSecond b
                    return $ (h1 && s1 == BoxAuto) || (h2 && s2 == BoxAuto)

        , keyEventHandler =
            \this key mods -> do
              b <- getState this
              handled <- handleKeyEvent (boxFirst b) key mods
              if handled then return True else
                  handleKeyEvent (boxSecond b) key mods

        , render_ = \this s ctx -> do
                      b <- getState this
                      renderBox s ctx b

        , getCursorPosition_ =
            \this -> do
              b <- getState this
              ch1_pos <- getCursorPosition $ boxFirst b
              case ch1_pos of
                Just v -> return $ Just v
                Nothing -> getCursorPosition $ boxSecond b

        , setCurrentPosition_ =
            \this pos -> do
              b <- getState this
              ch1_size <- getCurrentSize $ boxFirst b
              setCurrentPosition (boxFirst b) pos
              case boxOrientation b of
                Horizontal -> setCurrentPosition (boxSecond b) $
                              pos `plusWidth` ((fst ch1_size) + (toEnum $ boxSpacing b))
                Vertical -> setCurrentPosition (boxSecond b) $
                            pos `plusHeight` ((snd ch1_size) + (toEnum $ boxSpacing b))
        }

  wRef `relayFocusEvents` wa
  wRef `relayFocusEvents` wb

  return wRef

-- |Get a reference to the first (left or top) widget in a box.
getFirstChild :: Widget (Box a b) -> IO (Widget a)
getFirstChild = (boxFirst <~~)

-- |Get a reference to the second (right or bottom) widget in a box.
getSecondChild :: Widget (Box a b) -> IO (Widget b)
getSecondChild = (boxSecond <~~)

-- |Set the spacing in between a box's child widgets in rows or
-- columns, depending on the box type.
setBoxSpacing :: Widget (Box a b) -> Int -> IO ()
setBoxSpacing wRef spacing =
    updateWidgetState wRef $ \b -> b { boxSpacing = spacing }

withBoxSpacing :: Int -> Widget (Box a b) -> IO (Widget (Box a b))
withBoxSpacing spacing wRef = do
  setBoxSpacing wRef spacing
  return wRef

-- |Get the child size policy for a box.
getBoxChildSizePolicy :: Widget (Box a b) -> IO ChildSizePolicy
getBoxChildSizePolicy = (boxChildSizePolicy <~~)

-- |Set the box child size policy.  Throws 'BadPercentage' if the size
-- policy uses an invalid percentage value, which must be between 0
-- and 100 inclusive.
setBoxChildSizePolicy :: Widget (Box a b) -> ChildSizePolicy -> IO ()
setBoxChildSizePolicy b spol = do
  case spol of
    Percentage v -> when (v < 0 || v > 100) $ throw BadPercentage
    _ -> return ()

  updateWidgetState b $ \s -> s { boxChildSizePolicy = spol }

-- Box layout rendering implementation. This is generalized over the
-- two dimensions in which box layout can be performed; it takes lot
-- of functions, but mostly those are to query and update the correct
-- dimensions on regions and images as they are manipulated by the
-- layout algorithm.
renderBox :: (Show a, Show b) =>
             DisplayRegion
          -> RenderContext
          -> Box a b
          -> IO Image
renderBox s ctx this = do
  let actualSpace = regDimension this s - (toEnum (boxSpacing this))

  (img1, img2) <-
      -- XXX fix for case where we don't have enough space to honor
      -- hard-coded sizes (either fixed or derived fixed)

      -- XXX also check for overflow
      case boxChildSizePolicy this of
        PerChild BoxAuto BoxAuto -> renderBoxAuto s ctx this
        Percentage v -> do
                         let firstDim = round (fromRational
                                        (fromRational ((toRational v) / (100.0)) *
                                                          (toRational actualSpace)) ::Rational)
                             secondDim = fromEnum (actualSpace - firstDim)
                         renderBoxFixed s ctx this (fromEnum firstDim) secondDim
        PerChild BoxAuto (BoxFixed v) -> do
                                     let remaining = fromEnum (actualSpace - toEnum v)
                                     renderBoxFixed s ctx this remaining v
        PerChild (BoxFixed v) BoxAuto -> do
                                     let remaining = fromEnum (actualSpace - toEnum v)
                                     renderBoxFixed s ctx this v remaining
        PerChild (BoxFixed v1) (BoxFixed v2) -> renderBoxFixed s ctx this v1 v2

  let spAttr = getNormalAttr ctx
      spacing = boxSpacing this
      spacer = case spacing of
                 0 -> emptyImage
                 _ -> case boxOrientation this of
                         Horizontal -> let h = max (imageHeight img1) (imageHeight img2)
                                       in charFill spAttr ' ' (toEnum spacing) h
                         Vertical -> let w = max (imageWidth img1) (imageWidth img2)
                                     in charFill spAttr ' ' w (toEnum spacing)

      -- Use the larger of the two images to determine padding in the
      -- opposite dimension.  E.g. if this is a vertical box, we want
      -- to pad the images such that they have the same width.
      common_opposite_dim = case boxOrientation this of
                              Horizontal -> max (imageHeight img1) (imageHeight img2)
                              Vertical -> max (imageWidth img1) (imageWidth img2)

      padded_img1 = case boxOrientation this of
                      Horizontal -> img1 <->
                                    (charFill spAttr ' ' (imageWidth img1)
                                     (common_opposite_dim - imageHeight img1))
                      Vertical -> img1 <|>
                                  (charFill spAttr ' ' (common_opposite_dim - imageWidth img1)
                                   (imageHeight img1))
      padded_img2 = case boxOrientation this of
                      Horizontal -> img2 <->
                                    (charFill spAttr ' ' (imageWidth img2)
                                     (common_opposite_dim - imageHeight img2))
                      Vertical -> img2 <|>
                                  (charFill spAttr ' ' (common_opposite_dim - imageWidth img2)
                                   (imageHeight img2))


  return $ (img_cat this) [padded_img1, spacer, padded_img2]

renderBoxFixed :: (Show a, Show b) =>
                  DisplayRegion
               -> RenderContext
               -> Box a b
               -> Int
               -> Int
               -> IO (Image, Image)
renderBoxFixed s ctx this firstDim secondDim
    -- If the box is too large to fit in the available space (since it
    -- has fixed dimensions and can't be scaled), return the empty
    -- image.
    | toEnum firstDim + toEnum secondDim > regDimension this s = return (emptyImage, emptyImage)
    | otherwise = do
  let withDim = withDimension this
  img1 <- render (boxFirst this) (s `withDim` (toEnum firstDim)) ctx
  img2 <- render (boxSecond this) (s `withDim` (toEnum secondDim)) ctx

  -- pad the images so they fill the space appropriately.
  let fill img amt = case boxOrientation this of
                   Vertical -> charFill (getNormalAttr ctx) ' ' (imageWidth img) amt
                   Horizontal -> charFill (getNormalAttr ctx) ' ' amt (imageHeight img)
      firstDimW = toEnum firstDim
      secondDimW = toEnum secondDim
      img1_size = (imgDimension this) img1
      img2_size = (imgDimension this) img2
      img1_padded = if img1_size < firstDimW
                    then (img_cat this) [img1, fill img1 (firstDimW - img1_size)]
                    else img1
      img2_padded = if img2_size < secondDimW
                    then (img_cat this) [img2, fill img2 (secondDimW - img2_size)]
                    else img2

  return (img1_padded, img2_padded)

renderBoxAuto :: (Show a, Show b) =>
                 DisplayRegion
              -> RenderContext
              -> Box a b
              -> IO (Image, Image)
renderBoxAuto s ctx this = do
  let spacing = boxSpacing this
      first = boxFirst this
      second = boxSecond this
      withDim = withDimension this
      renderDimension = imgDimension this
      regDim = regDimension this

      actualSpace = s `withDim` (max (regDim s - toEnum spacing) 0)

      renderOrdered a b = do
        a_img <- render a actualSpace ctx

        let remaining = regDim actualSpace - renderDimension a_img
            s' = actualSpace `withDim` remaining

        b_img <- render b s' ctx

        return $ if renderDimension a_img >= regDim actualSpace
                 then [a_img, emptyImage]
                 else [a_img, b_img]

      renderHalves = do
        let half = actualSpace `withDim` div (regDim actualSpace) 2
            half' = if regDim actualSpace `mod` 2 == 0
                    then half
                    else half `withDim` (regDim half + 1)
        first_img <- render first half ctx
        second_img <- render second half' ctx
        return [first_img, second_img]

  gf <- firstGrows this
  gs <- secondGrows this

  [img1, img2] <- case (gf, gs) of
                    (True, True) -> renderHalves
                    (False, _) -> renderOrdered first second
                    (_, False) -> do
                                  images <- renderOrdered second first
                                  return $ reverse images

  return (img1, img2)
