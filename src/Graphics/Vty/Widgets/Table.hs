{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Graphics.Vty.Widgets.Table
    ( Table
    , TableCell(EmptyCell)
    , ColumnSize(..)
    , BorderStyle(..)
    , BorderFlag(..)
    , RowLike
    , TableError(..)
    , (.|.)
    , newTable
    , addRow
    , addHeadingRow
    , addHeadingRow_
    )
where

import Data.Typeable
    ( Typeable
    )
import Data.Word
    ( Word
    )
import Data.List
    ( intersperse
    )
import Control.Applicative
    ( (<$>)
    )
import Control.Exception
    ( Exception
    , throw
    )
import Control.Monad
    ( when
    , forM
    , forM_
    )
import Control.Monad.Trans
    ( MonadIO
    )
import Control.Monad.Reader
    ( ask
    )
import Graphics.Vty
    ( Image
    , Attr
    , DisplayRegion(..)
    , (<|>)
    , region_height
    , region_width
    , image_height
    , image_width
    , char_fill
    , vert_cat
    , horiz_cat
    , def_attr
    , empty_image
    , string
    )
import Graphics.Vty.Widgets.Core
    ( Widget
    , WidgetImpl(..)
    , (<~~)
    , render
    , newWidget
    , updateWidget
    , updateWidgetState
    , withWidth
    , setPhysicalPosition
    , getPhysicalSize
    , growVertical
    )
import Graphics.Vty.Widgets.Text
    ( FormattedText
    , simpleText
    )

data TableError = ColumnCountMismatch
                | CellImageTooBig
                | BadWidgetSizePolicy Int
                  deriving (Show, Typeable)

instance Exception TableError

data TableCell = forall a. TableCell (Widget a)
               | EmptyCell

data TableRow = TableRow [TableCell]

data BorderFlag = Rows | Columns | Edges
                  deriving (Eq, Show)

data BorderStyle = BorderPartial [BorderFlag]
                 | BorderFull
                 | BorderNone
                   deriving (Eq, Show)

data ColumnSize = Fixed Int | Auto
                  deriving (Eq, Show)

class RowLike a where
    mkRow :: a -> TableRow

instance RowLike TableRow where
    mkRow = id

instance RowLike TableCell where
    mkRow c = TableRow [c]

instance RowLike (Widget a) where
    mkRow = TableRow . (:[]) . TableCell

instance (RowLike a) => RowLike [a] where
    mkRow rs = TableRow cs
        where
          cs = concat $ map (\(TableRow cells) -> cells) rs'
          rs' = map mkRow rs

(.|.) :: (RowLike a, RowLike b) => a -> b -> TableRow
(.|.) a b = TableRow (cs ++ ds)
    where
      (TableRow cs) = mkRow a
      (TableRow ds) = mkRow b

data Table = Table { rows :: [TableRow]
                   , numColumns :: Int
                   , columnSizes :: [ColumnSize]
                   , borderStyle :: BorderStyle
                   , borderAttr :: Attr
                   }

newTable :: (MonadIO m) =>
            Attr
         -> [ColumnSize]
         -> BorderStyle
         -> m (Widget Table)
newTable attr sizes borderSty = do
  t <- newWidget
  updateWidget t $ \w ->
      w { state = Table { rows = []
                        , columnSizes = sizes
                        , borderStyle = borderSty
                        , numColumns = length sizes
                        , borderAttr = attr
                        }

        , getGrowHorizontal = do
            st <- ask
            return $ any (== Auto) (columnSizes st)

        , getGrowVertical = return False

        , draw =
            \this sz mAttr -> do
              rs <- rows <~~ this

              rowImgs <- mapM (\(TableRow r) -> renderRow this sz r mAttr) rs

              rowBorder <- mkRowBorder this sz
              topBottomBorder <- mkTopBottomBorder this sz
              sideBorder <- mkSideBorder this

              let body = vert_cat $ intersperse rowBorder rowImgs
                  withTBBorders = vert_cat [topBottomBorder, body, topBottomBorder]
                  withSideBorders = horiz_cat [sideBorder, withTBBorders, sideBorder]

              -- XXX only cat rows until we exceed the available space
              return withSideBorders

        , setPosition =
            \this pos -> do
              bs <- borderStyle <~~ this
              rs <- rows <~~ this

              let edgeOffset = if edgeBorders bs
                               then 1 else 0

                  positionRows _ [] = return ()
                  positionRows height ((TableRow row):rest) =
                    do
                      -- Compute the position for this row based on
                      -- border settings
                      let rowPos = DisplayRegion (region_width pos + edgeOffset)
                                   height

                      -- Get the maximum cell height
                      cellPhysSizes <- forM row $ \cell ->
                                       case cell of
                                         TableCell cw -> getPhysicalSize cw
                                         EmptyCell -> return $ DisplayRegion 0 1

                      -- Include 1 as a possible height to prevent
                      -- zero-height images from breaking position
                      -- computations.  This won't hurt in the case
                      -- where other cells are bigger, since their
                      -- heights will be chosen instead.
                      let maxSize = maximum $ 1 : map region_height cellPhysSizes
                          borderOffset = if rowBorders bs
                                         then 1 else 0

                      -- Position the individual row widgets (again,
                      -- based on border settings)
                      positionRow this bs rowPos row
                      positionRows (height + maxSize + borderOffset) rest

              -- XXX only position rendered rows
              positionRows (region_height pos + edgeOffset) rs

              return ()
        }
  return t

mkRowBorder :: Widget Table -> DisplayRegion-> IO Image
mkRowBorder t sz = do
  bs <- borderStyle <~~ t

  if not $ rowBorders bs then
      return empty_image else
      mkRowBorder_ t sz

-- Make a row border that matches the width of each row but does not
-- include outermost edge characters.
mkRowBorder_ :: Widget Table -> DisplayRegion -> IO Image
mkRowBorder_ t sz = do
  bs <- borderStyle <~~ t
  bAttr <- borderAttr <~~ t
  szs <- columnSizes <~~ t
  aw <- autoWidth t sz

  let intersection = string bAttr "+"
      imgs = (flip map) szs $ \s ->
             case s of
               Fixed n -> char_fill bAttr '-' n 1
               Auto -> char_fill bAttr '-' aw 1
      imgs' = if colBorders bs
              then intersperse intersection imgs
              else imgs

  return $ horiz_cat imgs'

mkTopBottomBorder :: Widget Table -> DisplayRegion -> IO Image
mkTopBottomBorder t sz = do
  bs <- borderStyle <~~ t

  if edgeBorders bs then
      mkRowBorder_ t sz else
      return empty_image

-- Make vertical side borders for the table, including row border
-- intersections if necessary.
mkSideBorder :: Widget Table -> IO Image
mkSideBorder t = do
  bs <- borderStyle <~~ t

  if edgeBorders bs then
      mkSideBorder_ t else
      return empty_image

mkSideBorder_ :: Widget Table -> IO Image
mkSideBorder_ t = do
  bs <- borderStyle <~~ t
  bAttr <- borderAttr <~~ t
  rs <- rows <~~ t

  let intersection = string bAttr "+"

  rowHeights <- forM rs $ \(TableRow row) -> do
                    hs <- forM row $ \cell ->
                          case cell of
                            TableCell cw -> region_height <$> getPhysicalSize cw
                            EmptyCell -> return 1
                    return $ maximum hs

  let borderImgs = (flip map) rowHeights $ \h -> char_fill bAttr '|' 1 h
      withIntersections = if rowBorders bs
                          then intersperse intersection borderImgs
                          else borderImgs

  return $ vert_cat $ intersection : withIntersections ++ [intersection]

positionRow :: Widget Table -> BorderStyle -> DisplayRegion -> [TableCell] -> IO ()
positionRow t bs pos cells = do
  -- Position each cell widget based on the base position of the row
  -- (which starts from the origin of the leftmost widget, NOT the
  -- leftmost cell border)
  oldSize <- getPhysicalSize t
  aw <- autoWidth t oldSize
  szs <- columnSizes <~~ t

  let offset = if colBorders bs
               then 1
               else 0

      cellWidth Auto = aw
      cellWidth (Fixed n) = toEnum n

      doPositioning _ [] = return ()
      doPositioning width ((szPolicy, cell):ws) =
          do
            case cell of
              TableCell w -> setPhysicalPosition w $ pos `withWidth` (region_width pos + width)
              EmptyCell -> return ()
            doPositioning (width + cellWidth szPolicy + offset) ws

  doPositioning 0 $ zip szs cells

autoWidth :: (MonadIO m) => Widget Table -> DisplayRegion -> m Word
autoWidth t sz = do
  sizes <- columnSizes <~~ t
  bs <- borderStyle <~~ t

  let numAuto = length $ filter (== Auto) sizes
      totalFixed = sum $ (flip map) sizes $ \s ->
                   case s of
                     Auto -> 0
                     Fixed n -> n
      edgeWidth = if edgeBorders bs then 2 else 0
      colWidth = if colBorders bs then (toEnum $ length sizes - 1) else 0

  return ((region_width sz - toEnum totalFixed - edgeWidth - colWidth) `div` toEnum numAuto)

addHeadingRow :: (MonadIO m) => Widget Table -> Attr -> [String] -> m [Widget FormattedText]
addHeadingRow tbl attr labels = do
  ws <- mapM (simpleText attr) labels
  addRow tbl ws
  return ws

addHeadingRow_ :: (MonadIO m) => Widget Table -> Attr -> [String] -> m ()
addHeadingRow_ tbl attr labels = addHeadingRow tbl attr labels >> return ()

addRow :: (MonadIO m, RowLike a) => Widget Table -> a -> m ()
addRow t row = do
  let (TableRow cells) = mkRow row

  forM_ (zip [1..] cells) $ \(i, c) -> do
                 case c of
                   EmptyCell -> return ()
                   TableCell w -> do
                          v <- growVertical w
                          when (v) $ throw $ BadWidgetSizePolicy i

  nc <- numColumns <~~ t
  when (length cells /= nc) $ throw ColumnCountMismatch

  updateWidgetState t $ \s ->
      s { rows = rows s ++ [TableRow cells] }

renderCell :: DisplayRegion -> TableCell -> Maybe Attr -> IO Image
renderCell region EmptyCell mAttr = do
  w <- simpleText def_attr ""
  render w region mAttr
renderCell region (TableCell w) mAttr = render w region mAttr

colBorders :: BorderStyle -> Bool
colBorders (BorderPartial fs) = Columns `elem` fs
colBorders BorderFull = True
colBorders _ = False

edgeBorders :: BorderStyle -> Bool
edgeBorders (BorderPartial fs) = Edges `elem` fs
edgeBorders BorderFull = True
edgeBorders _ = False

rowBorders :: BorderStyle -> Bool
rowBorders (BorderPartial fs) = Rows `elem` fs
rowBorders BorderFull = True
rowBorders _ = False

rowHeight :: [Image] -> Word
rowHeight = maximum . map image_height

renderRow :: Widget Table -> DisplayRegion -> [TableCell] -> Maybe Attr -> IO Image
renderRow tbl sz cells mAttr = do
  sizes <- columnSizes <~~ tbl
  borderSty <- borderStyle <~~ tbl
  bAttr <- borderAttr <~~ tbl
  aw <- autoWidth tbl sz

  cellImgs <-
      forM (zip cells sizes) $ \(cellW, sizeSpec) ->
          do
            let cellSz = DisplayRegion cellWidth (region_height sz)
                cellWidth = case sizeSpec of
                              Fixed n -> toEnum n
                              Auto -> aw

            img <- renderCell cellSz cellW mAttr
            -- Right-pad the image if it isn't big enough to fill the
            -- cell.
            case compare (image_width img) (region_width cellSz) of
              EQ -> return img
              -- XXX probably want a different attribute here, AND
              -- mAttr if it's set
              LT -> return $ img <|> char_fill def_attr ' '
                                     (region_width cellSz - image_width img)
                                     (max (image_height img) 1)
              GT -> throw CellImageTooBig

  -- If we need to draw borders in between columns, do that.
  let withBorders = case colBorders borderSty of
                      False -> cellImgs
                      True -> intersperse (char_fill bAttr '|' 1 (rowHeight cellImgs)) cellImgs

  return $ horiz_cat withBorders