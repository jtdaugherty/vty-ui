-- |This module provides a two-dimensional text zipper data structure.
-- This structure represents a body of text and an editing cursor
-- which can be moved throughout the text, along with a set of editing
-- transformations.
--
-- Text zippers are generalized over the set of data types that might
-- be used to store lists of characters (e.g., 'String', 'T.Text',
-- etc.).  As a result, the most general way to create a text zipper
-- is to use 'mkZipper' and provide all of the functions required to
-- manipulate the underlying text data.
--
-- A default implementation using 'T.Text' is provided and is used
-- elsewhere in this library.
module Graphics.Vty.Widgets.TextZipper
    ( TextZipper

    -- *Construction and extraction
    , mkZipper
    , textZipper
    , getText
    , currentLine
    , cursorPosition

    -- *Navigation functions
    , moveCursor
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

-- |Create a zipper using a custom text storage type.  Takes the
-- initial text as well as all of the functions necessary to
-- manipulate the underlying text values.
mkZipper :: (Monoid a) =>
            (Char -> a)
         -- ^A singleton constructor.
         -> (Int -> a -> a)
         -- ^'drop'.
         -> (Int -> a -> a)
         -- ^'take'.
         -> (a -> Int)
         -- ^'length'.
         -> (a -> Char)
         -- ^'last'.
         -> (a -> a)
         -- ^'init'.
         -> (a -> Bool)
         -- ^'null'.
         -> [a]
         -- ^The initial lines of text.
         -> TextZipper a
mkZipper fromCh drp tk lngth lst int nl ls =
    let (first, rest) = if null ls
                        then (mempty, mempty)
                        else (head ls, tail ls)
    in TZ mempty first [] rest fromCh drp tk lngth lst int nl

-- |Get the text contents of the zipper.
getText :: (Monoid a) => TextZipper a -> [a]
getText tz = concat [ above tz
                    , [currentLine tz]
                    , below tz
                    ]

-- |Get the cursor position of the zipper; returns @(row, col)@.
-- @row@ ranges from @[0..num_rows-1]@ inclusive; @col@ ranges from
-- @[0..length of current line]@ inclusive.  Column values equal to
-- line width indicate a cursor that is just past the end of a line of
-- text.
cursorPosition :: TextZipper a -> (Int, Int)
cursorPosition tz = (length $ above tz, length_ tz $ toLeft tz)

-- |Move the cursor to the specified row and column.  Invalid cursor
-- positions will be ignored.  Valid cursor positions range as
-- described for 'cursorPosition'.
moveCursor :: (Monoid a) => (Int, Int) -> TextZipper a -> TextZipper a
moveCursor (row, col) tz =
    let t = getText tz
    in if row < 0
           || row > length t
           || col < 0
           || col > length_ tz (t !! row)
       then tz
       else tz { above = take row t
               , below = drop (row + 1) t
               , toLeft = take_ tz col (t !! row)
               , toRight = drop_ tz col (t !! row)
               }

lastLine :: TextZipper a -> Bool
lastLine = (== 0) . length . below

nextLine :: TextZipper a -> a
nextLine = head . below

-- |The line of text on which the zipper's cursor currently resides.
currentLine :: (Monoid a) => TextZipper a -> a
currentLine tz = (toLeft tz) `mappend` (toRight tz)

-- |Insert a character at the current cursor position.
insertChar :: (Monoid a) => Char -> TextZipper a -> TextZipper a
insertChar ch tz = tz { toLeft = toLeft tz `mappend` (fromChar tz ch) }

-- |Insert a line break at the current cursor position.
breakLine :: (Monoid a) => TextZipper a -> TextZipper a
breakLine tz =
    tz { above = above tz ++ [toLeft tz]
       , toLeft = mempty
       }

-- |Move the cursor to the end of the current line.
gotoEOL :: (Monoid a) => TextZipper a -> TextZipper a
gotoEOL tz = tz { toLeft = currentLine tz
                , toRight = mempty
                }

-- |Remove all text from the cursor position to the end of the current
-- line.  If the cursor is at the beginning of a line and the line is
-- empty, the entire line will be removed.
killToEOL :: (Monoid a) => TextZipper a -> TextZipper a
killToEOL tz
    | (null_ tz $ toLeft tz) && (null_ tz $ toRight tz) &&
      (not $ null $ below tz) =
          tz { toRight = head $ below tz
             , below = tail $ below tz
             }
    | otherwise = tz { toRight = mempty
                     }

-- |Delete the character preceding the cursor position, and move the
-- cursor backwards by one character.
deletePrevChar :: (Eq a, Monoid a) => TextZipper a -> TextZipper a
deletePrevChar tz
    | moveLeft tz == tz = tz
    | otherwise = deleteChar $ moveLeft tz

-- |Delete the character at the cursor position.  Leaves the cursor
-- position unchanged.  If the cursor is at the end of a line of text,
-- this combines the line with the line below.
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

-- |Move the cursor to the beginning of the current line.
gotoBOL :: (Monoid a) => TextZipper a -> TextZipper a
gotoBOL tz = tz { toLeft = mempty
                , toRight = currentLine tz
                }

-- |Move the cursor right by one position.  If the cursor is at the
-- end of a line, the cursor is moved to the first position of the
-- following line (if any).
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

-- |Move the cursor left by one position.  If the cursor is at the
-- beginning of a line, the cursor is moved to the last position of
-- the preceding line (if any).
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

-- |Move the cursor up by one row.  If there no are rows above the
-- current one, move to the first position of the current row.  If the
-- row above is shorter, move to the end of that row.
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

-- |Move the cursor down by one row.  If there are no rows below the
-- current one, move to the last position of the current row.  If the
-- row below is shorter, move to the end of that row.
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

-- |Construct a zipper from 'T.Text' values.
textZipper :: [T.Text] -> TextZipper T.Text
textZipper =
    mkZipper T.singleton T.drop T.take T.length T.last T.init T.null
