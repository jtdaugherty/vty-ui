module Graphics.Vty.Widgets.TextZipper
    ( TextZipper

    -- *Construction and extraction
    , textZipper
    , getText
    , currentLine
    , cursorPosition

    -- *Navigation functions
    , insertChar
    , breakLine
    , killToEOL
    , gotoEOL
    , gotoBOL
    , deletePrevChar
    , deleteChar
    , moveRight
    , moveLeft
    , moveUp
    , moveDown
    )
where

import Data.Monoid
import qualified Data.Text as T

data TextZipper a =
    TZ { toLeft :: a
       , toRight :: a
       , above :: [a]
       , below :: [a]
       , fromChar :: Char -> a
       , drop_ :: Int -> a -> a
       , take_ :: Int -> a -> a
       , length_ :: a -> Int
       , last_ :: a -> Char
       , init_ :: a -> a
       , null_ :: a -> Bool
       }

instance (Eq a) => Eq (TextZipper a) where
    a == b = and [ toLeft a == toLeft b
                 , toRight a == toRight b
                 , above a == above b
                 , below a == below b
                 ]

instance (Show a) => Show (TextZipper a) where
    show tz = concat [ "TextZipper { "
                     , "above = "
                     , show $ above tz
                     , "below = "
                     , show $ below tz
                     , "toLeft = "
                     , show $ toLeft tz
                     , "toRight = "
                     , show $ toRight tz
                     , " }"
                     ]

mkZipper :: (Monoid a) =>
            [a]
         -> (Char -> a)
         -> (Int -> a -> a)
         -> (Int -> a -> a)
         -> (a -> Int)
         -> (a -> Char)
         -> (a -> a)
         -> (a -> Bool)
         -> TextZipper a
mkZipper ls fromCh drp tk lngth lst int nl =
    let (first, rest) = if null ls
                        then (mempty, mempty)
                        else (head ls, tail ls)
    in TZ mempty first [] rest fromCh drp tk lngth lst int nl

getText :: (Monoid a) => TextZipper a -> [a]
getText tz = concat [ above tz
                    , [currentLine tz]
                    , below tz
                    ]

-- |Returns (row, col)
cursorPosition :: TextZipper a -> (Int, Int)
cursorPosition tz = (length $ above tz, length_ tz $ toLeft tz)

lastLine :: TextZipper a -> Bool
lastLine = (== 0) . length . below

nextLine :: TextZipper a -> a
nextLine = head . below

currentLine :: (Monoid a) => TextZipper a -> a
currentLine tz = (toLeft tz) `mappend` (toRight tz)

insertChar :: (Monoid a) => Char -> TextZipper a -> TextZipper a
insertChar ch tz = tz { toLeft = toLeft tz `mappend` (fromChar tz ch) }

breakLine :: (Monoid a) => TextZipper a -> TextZipper a
breakLine tz =
    tz { above = above tz ++ [toLeft tz]
       , toLeft = mempty
       }

gotoEOL :: (Monoid a) => TextZipper a -> TextZipper a
gotoEOL tz = tz { toLeft = currentLine tz
                , toRight = mempty
                }

killToEOL :: (Monoid a) => TextZipper a -> TextZipper a
killToEOL tz
    | (null_ tz $ toLeft tz) && (null_ tz $ toRight tz) &&
      (not $ null $ below tz) =
          tz { toRight = head $ below tz
             , below = tail $ below tz
             }
    | otherwise = tz { toRight = mempty
                     }

deletePrevChar :: (Eq a, Monoid a) => TextZipper a -> TextZipper a
deletePrevChar tz
    | moveLeft tz == tz = tz
    | otherwise = deleteChar $ moveLeft tz

deleteChar :: (Monoid a) => TextZipper a -> TextZipper a
deleteChar tz
    -- Can we just remove a char from the current line?
    | (not $ null_ tz (toRight tz)) =
        tz { toRight = drop_ tz 1 $ toRight tz
           }
    -- Do we need to collapse the previous line onto the current one?
    | null_ tz (toRight tz) && (not $ null $ below tz) =
        tz { toRight = head $ below tz
           , below = tail $ below tz
           }
    | otherwise = tz

gotoBOL :: (Monoid a) => TextZipper a -> TextZipper a
gotoBOL tz = tz { toLeft = mempty
                , toRight = currentLine tz
                }

moveRight :: (Monoid a) => TextZipper a -> TextZipper a
moveRight tz
    -- Are we able to keep moving right on the current line?
    | not (null_ tz (toRight tz)) =
        tz { toLeft = toLeft tz
                      `mappend` (take_ tz 1 $ toRight tz)
           , toRight = drop_ tz 1 (toRight tz)
           }
    -- If we are going to go beyond the end of the current line, can
    -- we move to the next one?
    | not $ null (below tz) =
        tz { above = above tz ++ [toLeft tz]
           , below = tail $ below tz
           , toLeft = mempty
           , toRight = nextLine tz
           }
    | otherwise = tz

moveLeft :: (Monoid a) => TextZipper a -> TextZipper a
moveLeft tz
    -- Are we able to keep moving left on the current line?
    | not $ null_ tz (toLeft tz) =
        tz { toLeft = init_ tz $ toLeft tz
           , toRight = fromChar tz (last_ tz (toLeft tz))
                       `mappend` toRight tz
           }
    -- If we are going to go beyond the beginning of the current line,
    -- can we move to the end of the previous one?
    | not $ null (above tz) =
        tz { above = init $ above tz
           , below = currentLine tz : below tz
           , toLeft = last $ above tz
           , toRight = mempty
           }
    | otherwise = tz

moveUp :: (Monoid a) => TextZipper a -> TextZipper a
moveUp tz
    -- Is there a line above at least as long as the current one?
    | (not $ null (above tz)) &&
      (length_ tz $ last $ above tz) >= length_ tz (toLeft tz) =
        tz { below = currentLine tz : below tz
           , above = init $ above tz
           , toLeft = take_ tz (length_ tz $ toLeft tz) (last $ above tz)
           , toRight = drop_ tz (length_ tz $ toLeft tz) (last $ above tz)
           }
    -- Or if there is a line above, just go to the end of it
    | (not $ null (above tz)) =
        tz { above = init $ above tz
           , below = currentLine tz : below tz
           , toLeft = last $ above tz
           , toRight = mempty
           }
    -- If nothing else, go to the beginning of the current line
    | otherwise = gotoBOL tz

moveDown :: (Monoid a) => TextZipper a -> TextZipper a
moveDown tz
    -- Is there a line below at least as long as the current one?
    | (not $ lastLine tz) &&
      (length_ tz $ nextLine tz) >= length_ tz (toLeft tz) =
        tz { below = tail $ below tz
           , above = above tz ++ [currentLine tz]
           , toLeft = take_ tz (length_ tz $ toLeft tz) (nextLine tz)
           , toRight = drop_ tz (length_ tz $ toLeft tz) (nextLine tz)
           }
    -- Or if there is a line below, just go to the end of it
    | (not $ null (below tz)) =
        tz { above = above tz ++ [currentLine tz]
           , below = tail $ below tz
           , toLeft = nextLine tz
           , toRight = mempty
           }
    -- If nothing else, go to the end of the current line
    | otherwise = gotoEOL tz

textZipper :: [T.Text] -> TextZipper T.Text
textZipper ls =
    mkZipper ls T.singleton T.drop T.take T.length T.last T.init T.null
