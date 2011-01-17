{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Graphics.Vty.Widgets.Table
    ( Table
    , TableCell(EmptyCell)
    , ColumnSize(..)
    , BorderStyle(..)
    , BorderFlag(..)
    , RowLike
    , TableError(..)
    , ColumnSpec
    , (.|.)
    , newTable
    , setDefaultCellAlignment
    , setDefaultCellPadding
    , addRow
    , addHeadingRow
    , addHeadingRow_
    , column
    , customCell
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
    , growHorizontal
    )
import Graphics.Vty.Widgets.Text
    ( FormattedText
    , simpleText
    )
import Graphics.Vty.Widgets.Base
    ( (<++>)
    , hCentered
    , hFill
    )
import Graphics.Vty.Widgets.Padding
    ( Padding
    , Paddable(..)
    , padded
    , padNone
    )
import Graphics.Vty.Widgets.Alignment
    ( Alignable(..)
    , Alignment(..)
    )

data TableError = ColumnCountMismatch
                | CellImageTooBig
                | BadWidgetSizePolicy Int
                  deriving (Show, Typeable)

instance Exception TableError

data TableCell = forall a. TableCell (Widget a) (Maybe Alignment) (Maybe Padding)
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

data ColumnSpec = ColumnSpec { columnSize :: ColumnSize
                             , columnAlignment :: Maybe Alignment
                             , columnPadding :: Maybe Padding
                             }

instance Paddable ColumnSpec where
    pad c p = c { columnPadding = Just p }

instance Alignable ColumnSpec where
    align c a = c { columnAlignment = Just a }

instance Paddable TableCell where
    pad (TableCell w a _) p = TableCell w a (Just p)
    pad EmptyCell _ = EmptyCell

instance Alignable TableCell where
    align (TableCell w _ p) a = TableCell w (Just a) p
    align EmptyCell _ = EmptyCell

class RowLike a where
    mkRow :: a -> TableRow

instance RowLike TableRow where
    mkRow = id

instance RowLike TableCell where
    mkRow c = TableRow [c]

instance RowLike (Widget a) where
    mkRow w = TableRow [TableCell w Nothing Nothing]

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

infixl 2 .|.

data Table = Table { rows :: [TableRow]
                   , numColumns :: Int
                   , columnSpecs :: [ColumnSpec]
                   , borderStyle :: BorderStyle
                   , borderAttr :: Attr
                   , defaultCellAlignment :: Alignment
                   , defaultCellPadding :: Padding
                   }

customCell :: Widget a -> TableCell
customCell w = TableCell w Nothing Nothing

setDefaultCellAlignment :: (MonadIO m) => Widget Table -> Alignment -> m ()
setDefaultCellAlignment t a = updateWidgetState t $ \s -> s { defaultCellAlignment = a }

setDefaultCellPadding :: (MonadIO m) => Widget Table -> Padding -> m ()
setDefaultCellPadding t p = updateWidgetState t $ \s -> s { defaultCellPadding = p }

column :: ColumnSize -> ColumnSpec
column sz = ColumnSpec sz Nothing Nothing

newTable :: (MonadIO m) =>
            Attr
         -> [ColumnSpec]
         -> BorderStyle
         -> m (Widget Table)
newTable attr specs borderSty = do
  t <- newWidget
  updateWidget t $ \w ->
      w { state = Table { rows = []
                        , columnSpecs = specs
                        , borderStyle = borderSty
                        , numColumns = length specs
                        , borderAttr = attr
                        , defaultCellAlignment = AlignLeft
                        , defaultCellPadding = padNone
                        }

        , getGrowHorizontal = do
            st <- ask
            return $ any (== Auto) (map columnSize $ columnSpecs st)

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
                                         TableCell cw _ _ -> getPhysicalSize cw
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

getCellAlignment :: (MonadIO m) => Widget Table -> Int -> TableCell -> m Alignment
getCellAlignment _ _ (TableCell _ (Just p) _) = return p
getCellAlignment t columnNumber _ = do
  -- If the column for this cell has properties, use those; otherwise
  -- default to table-wide properties.
  specs <- columnSpecs <~~ t
  let spec = specs !! columnNumber

  case columnAlignment spec of
    Nothing -> defaultCellAlignment <~~ t
    Just p -> return p

getCellPadding :: (MonadIO m) => Widget Table -> Int -> TableCell -> m Padding
getCellPadding _ _ (TableCell _ _ (Just p)) = return p
getCellPadding t columnNumber _ = do
  -- If the column for this cell has properties, use those; otherwise
  -- default to table-wide properties.
  specs <- columnSpecs <~~ t
  let spec = specs !! columnNumber

  case columnPadding spec of
    Nothing -> defaultCellPadding <~~ t
    Just p -> return p

mkRowBorder :: Widget Table -> DisplayRegion -> IO Image
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
  specs <- columnSpecs <~~ t
  aw <- autoWidth t sz

  let szs = map columnSize specs
      intersection = string bAttr "+"
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
                            TableCell cw _ _ -> region_height <$> getPhysicalSize cw
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
  specs <- columnSpecs <~~ t

  let szs = map columnSize specs
      offset = if colBorders bs
               then 1
               else 0

      cellWidth Auto = aw
      cellWidth (Fixed n) = toEnum n

      doPositioning _ [] = return ()
      doPositioning width ((szPolicy, cell):ws) =
          do
            case cell of
              TableCell w _ _ -> setPhysicalPosition w $ pos `withWidth` (region_width pos + width)
              EmptyCell -> return ()
            doPositioning (width + cellWidth szPolicy + offset) ws

  doPositioning 0 $ zip szs cells

autoWidth :: (MonadIO m) => Widget Table -> DisplayRegion -> m Word
autoWidth t sz = do
  specs <- columnSpecs <~~ t
  bs <- borderStyle <~~ t

  let sizes = map columnSize specs
      numAuto = length $ filter (== Auto) sizes
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

applyCellAlignment :: (MonadIO m) => Alignment -> TableCell -> m TableCell
applyCellAlignment _ EmptyCell = return EmptyCell
applyCellAlignment al (TableCell w a p) = do
  case al of
    AlignLeft -> return $ TableCell w a p

    AlignCenter -> do
      -- XXX this check belongs in the centering code...
      grow <- growHorizontal w
      case grow of
        False -> do
                  w' <- hCentered w
                  return $ TableCell w' a p
        True -> return $ TableCell w a p

    AlignRight -> do
      grow <- growHorizontal w
      case grow of
        False -> do
                  w' <- hFill def_attr ' ' 1 <++> (return w)
                  return $ TableCell w' a p
        True -> return $ TableCell w a p

applyCellPadding :: (MonadIO m) => Padding -> TableCell -> m TableCell
applyCellPadding _ EmptyCell = return EmptyCell
applyCellPadding padding (TableCell w a p) = do
  w' <- padded w padding
  return $ TableCell w' a p

addRow :: (MonadIO m, RowLike a) => Widget Table -> a -> m ()
addRow t row = do
  let (TableRow cells_) = mkRow row

  cells <- forM (zip [1..] cells_) $ \(i, c) -> do
                 case c of
                   EmptyCell -> return ()
                   TableCell w _ _ -> do
                          v <- growVertical w
                          when (v) $ throw $ BadWidgetSizePolicy i

                 -- Apply cell properties to the widget in this cell.
                 alignment <- getCellAlignment t (i - 1) c
                 padding <- getCellPadding t (i - 1) c

                 applyCellAlignment alignment c >>= applyCellPadding padding

  nc <- numColumns <~~ t
  when (length cells /= nc) $ throw ColumnCountMismatch

  updateWidgetState t $ \s ->
      s { rows = rows s ++ [TableRow cells] }

renderCell :: DisplayRegion -> TableCell -> Maybe Attr -> IO Image
renderCell region EmptyCell mAttr = do
  w <- simpleText def_attr ""
  render w region mAttr
renderCell region (TableCell w _ _) mAttr = render w region mAttr

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
  specs <- columnSpecs <~~ tbl
  borderSty <- borderStyle <~~ tbl
  bAttr <- borderAttr <~~ tbl
  aw <- autoWidth tbl sz

  let sizes = map columnSize specs

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