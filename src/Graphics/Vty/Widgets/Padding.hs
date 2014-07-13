{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}
-- |This module provides a ''padding'' mechanism for adding padding to
-- a widget on one or more sides.
module Graphics.Vty.Widgets.Padding
    ( Padded
    , Padding
    , Paddable(..)
    , (+++)
    , padded
    , withPadding
    , padNone
    , padLeft
    , padRight
    , padTop
    , padBottom
    , padLeftRight
    , padTopBottom
    , padAll
    )
where

import Data.Word
import Data.Monoid
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

-- |The type of padding on widgets.
data Padding = Padding Int Int Int Int
               deriving (Show)

data Padded = forall a. (Show a) => Padded (Widget a) Padding

instance Show Padded where
    show (Padded _ p) = concat [ "Padded { "
                               , "padding = "
                               , show p
                               , ", ... }"
                               ]

instance Monoid Padding where
    mempty = Padding 0 0 0 0
    mappend (Padding a1 a2 a3 a4) (Padding b1 b2 b3 b4) =
        Padding (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)

(+++) :: (Monoid a) => a -> a -> a
(+++) = mappend

-- |The class of types to which we can add padding.
class Paddable a where
    pad :: a -> Padding -> a

instance Paddable Padding where
    pad p1 p2 = p1 +++ p2

leftPadding :: Padding -> Word
leftPadding (Padding _ _ _ l) = toEnum l

rightPadding :: Padding -> Word
rightPadding (Padding _ r _ _) = toEnum r

bottomPadding :: Padding -> Word
bottomPadding (Padding _ _ b _) = toEnum b

topPadding :: Padding -> Word
topPadding (Padding t _ _ _) = toEnum t

-- |Padding constructor with no padding.
padNone :: Padding
padNone = Padding 0 0 0 0

-- |Padding constructor with left padding in columns.
padLeft :: Int -> Padding
padLeft v = Padding 0 0 0 v

-- |Padding constructor with right padding in columns.
padRight :: Int -> Padding
padRight v = Padding 0 v 0 0

-- |Padding constructor with top padding in rows.
padTop :: Int -> Padding
padTop v = Padding v 0 0 0

-- |Padding constructor with bottom padding in rows.
padBottom :: Int -> Padding
padBottom v = Padding 0 0 v 0

-- |Padding constructor with padding on all sides in rows and
-- columns.
padAll :: Int -> Padding
padAll v = Padding v v v v

-- |Padding constructor with padding on top and bottom in rows.
padTopBottom :: Int -> Padding
padTopBottom v = Padding v 0 v 0

-- |Padding constructor with padding on left and right in columns.
padLeftRight :: Int -> Padding
padLeftRight v = Padding 0 v 0 v

-- |Monadic combinator to construct a 'Padded' wrapper.
withPadding :: (Show a) => Padding -> Widget a -> IO (Widget Padded)
withPadding = flip padded

-- |Create a 'Padded' wrapper to add padding.
padded :: (Show a) => Widget a -> Padding -> IO (Widget Padded)
padded ch padding = do
  let initSt = Padded ch padding
  wRef <- newWidget initSt $ \w ->
      w { growVertical_ = const $ growVertical ch
        , growHorizontal_ = const $ growHorizontal ch

        , render_ =
            \this sz ctx -> do
                Padded child p <- getState this

                if (region_width sz < leftPadding p + rightPadding p) ||
                   (region_height sz < bottomPadding p + topPadding p) then
                  return empty_image else
                 do
                  f <- focused <~ this

                  -- Compute constrained space based on padding
                  -- settings.
                  let constrained = sz `withWidth` (toEnum $ max 0 newWidth)
                                    `withHeight` (toEnum $ max 0 newHeight)
                      newWidth = (fromEnum $ region_width sz) - fromEnum (leftPadding p + rightPadding p)
                      newHeight = (fromEnum $ region_height sz) - fromEnum (topPadding p + bottomPadding p)
                      attr = mergeAttrs [ if f then focusAttr ctx else overrideAttr ctx
                                        , normalAttr ctx
                                        ]

                  -- Render child.
                  img <- render child constrained ctx

                  -- Create padding images.
                  let leftImg = char_fill attr ' ' (leftPadding p) (image_height img)
                      rightImg = char_fill attr ' ' (rightPadding p) (image_height img)
                      topImg = char_fill attr ' ' (image_width img + leftPadding p + rightPadding p)
                               (topPadding p)
                      bottomImg = char_fill attr ' ' (image_width img + leftPadding p + rightPadding p)
                                  (bottomPadding p)

                  return $ topImg <-> (leftImg <|> img <|> rightImg) <-> bottomImg

        , setCurrentPosition_ =
            \this pos -> do
              Padded child p <- getState this

              -- Considering left and top padding, adjust position and
              -- set on child.
              let newPos = pos
                           `plusWidth` (leftPadding p)
                           `plusHeight` (topPadding p)

              setCurrentPosition child newPos

        , getCursorPosition_ = \this -> do
              Padded child _ <- getState this
              getCursorPosition child
        }

  wRef `relayKeyEvents` ch
  wRef `relayFocusEvents` ch
  return wRef
