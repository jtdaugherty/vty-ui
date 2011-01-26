{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}
module Graphics.Vty.Widgets.Padding
    ( Padded
    , Padding
    , Paddable(..)
    , (+++)
    , padded
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
    ( Word
    )
import Data.Monoid
    ( Monoid(..)
    )
import Control.Monad.Trans
    ( MonadIO
    )
import Graphics.Vty
    ( Attr
    , (<->)
    , (<|>)
    , char_fill
    , image_width
    , image_height
    , region_width
    , region_height
    , def_attr
    , empty_image
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , HasNormalAttr(..)
    , RenderContext(..)
    , newWidget
    , updateWidget
    , updateWidgetState
    , growVertical
    , growHorizontal
    , handleKeyEvent
    , getState
    , render
    , setPhysicalPosition
    , onKeyPressed
    )
import Graphics.Vty.Widgets.Util

-- Top, right, bottom, left.
data Padding = Padding Int Int Int Int
               deriving (Show)

data Padded = forall a. (Show a) => Padded (Widget a) Padding Attr

instance Show Padded where
    show (Padded _ p mAttr) = concat [ "Padded { "
                                     , "padding = "
                                     , show p
                                     , ", paddingAttr = "
                                     , show mAttr
                                     , ", ... }"
                                     ]

instance Monoid Padding where
    mempty = Padding 0 0 0 0
    mappend (Padding a1 a2 a3 a4) (Padding b1 b2 b3 b4) =
        Padding (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)

(+++) :: (Monoid a) => a -> a -> a
(+++) = mappend

class Paddable a where
    pad :: a -> Padding -> a

instance Paddable Padding where
    pad p1 p2 = p1 +++ p2

instance HasNormalAttr (Widget Padded) where
    setNormalAttribute wRef a =
        updateWidgetState wRef $ \(Padded w p a') -> Padded w p (mergeAttr a a')

leftPadding :: Padding -> Word
leftPadding (Padding _ _ _ l) = toEnum l

rightPadding :: Padding -> Word
rightPadding (Padding _ r _ _) = toEnum r

bottomPadding :: Padding -> Word
bottomPadding (Padding _ _ b _) = toEnum b

topPadding :: Padding -> Word
topPadding (Padding t _ _ _) = toEnum t

-- Padding constructors
padNone :: Padding
padNone = Padding 0 0 0 0

padLeft :: Int -> Padding
padLeft v = Padding 0 0 0 v

padRight :: Int -> Padding
padRight v = Padding 0 v 0 0

padTop :: Int -> Padding
padTop v = Padding v 0 0 0

padBottom :: Int -> Padding
padBottom v = Padding 0 0 v 0

padAll :: Int -> Padding
padAll v = Padding v v v v

padTopBottom :: Int -> Padding
padTopBottom v = Padding v 0 v 0

padLeftRight :: Int -> Padding
padLeftRight v = Padding 0 v 0 v

padded :: (MonadIO m, Show a) => Widget a -> Padding -> m (Widget Padded)
padded ch padding = do
  wRef <- newWidget
  updateWidget wRef $ \w ->
      w { state = Padded ch padding def_attr

        , getGrowVertical = const $ growVertical ch
        , getGrowHorizontal = const $ growHorizontal ch

        , draw =
            \this sz ctx ->
                if (region_width sz < 2) || (region_height sz < 2)
                then return empty_image
                else do
                  Padded child p att <- getState this

                  -- Compute constrained space based on padding
                  -- settings.
                  let constrained = sz `withWidth` (toEnum $ max 0 newWidth)
                                    `withHeight` (toEnum $ max 0 newHeight)
                      newWidth = (fromEnum $ region_width sz) - fromEnum (leftPadding p + rightPadding p)
                      newHeight = (fromEnum $ region_height sz) - fromEnum (topPadding p + bottomPadding p)
                      attr = mergeAttrs [ overrideAttr ctx
                                        , att
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

        , setPosition =
            \this pos -> do
              Padded child p _ <- getState this

              -- Considering left and top padding, adjust position and
              -- set on child.
              let newPos = pos
                           `withWidth` (region_width pos + leftPadding p)
                           `withHeight` (region_height pos + topPadding p)

              setPhysicalPosition child newPos

        }

  wRef `onKeyPressed` \_ key mods -> handleKeyEvent ch key mods
  return wRef