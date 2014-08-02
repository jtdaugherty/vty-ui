{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
-- |This module provides a table layout widget capable of laying out
-- columns of widgets with various padding and alignment properties.
-- For complete details, please see the Vty-ui User's Manual.
module Graphics.Vty.Widgets.Table
    ( Table
    , TableCell
    , ColumnSize(..)
    , BorderStyle(..)
    , BorderFlag(..)
    , RowLike(..)
    , TableError(..)
    , ColumnSpec(..)
    , (.|.)
    , newTable
    , setDefaultCellAlignment
    , setDefaultCellPadding
    , addRow
    , addHeadingRow
    , addHeadingRow_
    , column
    , customCell
    , emptyCell
    )
where

import Data.Monoid
import qualified Data.Text as T
import Data.Typeable
import Data.List
import Control.Applicative hiding ((<|>))
import Control.Exception
import Control.Monad
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Centering
import Graphics.Vty.Widgets.Padding
import Graphics.Vty.Widgets.Borders
import Graphics.Vty.Widgets.Skins
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Fills
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Alignment

data TableError = ColumnCountMismatch
                -- ^A row added to the table did not have the same
                -- number of widgets as the table has columns.
                | CellImageTooBig
                -- ^The image rendered by a cell widget exceeded the
                -- size permitted by the cell.
                | BadTableWidgetSizePolicy Int
                  -- ^A table cell contains a widget which grows
                  -- vertically, which is not permitted.
                  deriving (Show, Typeable)

instance Exception TableError

-- |The wrapper type for all table cells; stores the widgets
-- themselves in addition to alignment and padding settings.
-- Alignment and padding settings on a cell override the column- and
-- table-wide defaults.
data TableCell = forall a. (Show a) => TableCell (Widget a) (Maybe Alignment) (Maybe Padding)
               | EmptyCell

instance Show TableCell where
    show EmptyCell = "EmptyCell"
    show (TableCell _ mAl mPad) = concat [ "TableCell { "
                                         , "alignment = "
                                         , show mAl
                                         , ", padding = "
                                         , show mPad
                                         , ", ... "
                                         , "}"
                                         ]

data TableRow = TableRow [TableCell]

-- |The types of borders we can have in a table.
data BorderFlag = Rows
                -- ^Borders between rows.
                | Columns
                -- ^Borders between columns.
                | Edges
                  -- ^Borders around the outside edges of the table.
                  deriving (Eq, Show)

-- |The border configuration of a table.
data BorderStyle = BorderPartial [BorderFlag]
                 -- |A partial set of border flags.
                 | BorderFull
                 -- |Draw borders everywhere we support them.
                 | BorderNone
                   -- ^Don't draw any borders anywhere.
                   deriving (Eq, Show)

-- |The type of column size policies.
data ColumnSize = ColFixed Int
                -- ^The column has the specified fixed width in
                -- columns.
                | ColAuto
                  -- ^The column's width is a function of space
                  -- available to the table at rendering time.
                  deriving (Eq, Show)

-- |The specification of a column's settings.  The alignment and
-- padding of a column specification override the table-wide default.
data ColumnSpec = ColumnSpec { columnSize :: ColumnSize
                             , columnAlignment :: Maybe Alignment
                             , columnPadding :: Maybe Padding
                             }
                  deriving (Show)

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

-- |The class of types whose values can be used to construct table
-- rows.
class RowLike a where
    mkRow :: a -> TableRow

instance RowLike TableRow where
    mkRow = id

instance RowLike TableCell where
    mkRow c = TableRow [c]

instance (Show a) => RowLike (Widget a) where
    mkRow w = TableRow [TableCell w Nothing Nothing]

instance (RowLike a) => RowLike [a] where
    mkRow rs = TableRow cs
        where
          cs = concat $ map (\(TableRow cells) -> cells) rs'
          rs' = map mkRow rs

-- |Row constructor using 'RowLike' instances.
(.|.) :: (RowLike a, RowLike b) => a -> b -> TableRow
(.|.) a b = TableRow (cs ++ ds)
    where
      (TableRow cs) = mkRow a
      (TableRow ds) = mkRow b

instance Monoid TableRow where
    mempty = TableRow []
    (TableRow as) `mappend` (TableRow bs) = TableRow $ as ++ bs

infixl 2 .|.

data Table = Table { rows :: [TableRow]
                   , numColumns :: Int
                   , columnSpecs :: [ColumnSpec]
                   , borderStyle :: BorderStyle
                   , borderAttr :: Attr
                   , defaultCellAlignment :: Alignment
                   , defaultCellPadding :: Padding
                   }

instance HasBorderAttr (Widget Table) where
    setBorderAttribute t a =
        updateWidgetState t $ \s -> s { borderAttr = mergeAttr a $ borderAttr s }

instance Show Table where
    show t = concat [ "Table { "
                    , "rows = <", show $ length $ rows t, " rows>"
                    , ", numColumns = ", show $ numColumns t
                    , ", columnSpecs = ", show $ columnSpecs t
                    , ", borderStyle = ", show $ borderStyle t
                    , ", borderAttr = ", show $ borderAttr t
                    , ", defaultCellAlignment = ", show $ defaultCellAlignment t
                    , ", defaultCellPadding = ", show $ defaultCellPadding t
                    , " }"
                    ]

-- |Create a custom 'TableCell' to set its alignment and/or padding
-- settings.
customCell :: (Show a) => Widget a -> TableCell
customCell w = TableCell w Nothing Nothing

-- |Create an empty table cell.
emptyCell :: TableCell
emptyCell = EmptyCell

-- |Set the default table-wide cell alignment.
setDefaultCellAlignment :: Widget Table -> Alignment -> IO ()
setDefaultCellAlignment t a = updateWidgetState t $ \s -> s { defaultCellAlignment = a }

-- |Set the default table-wide cell padding.
setDefaultCellPadding :: Widget Table -> Padding -> IO ()
setDefaultCellPadding t p = updateWidgetState t $ \s -> s { defaultCellPadding = p }

-- |Create a column.
column :: ColumnSize -> ColumnSpec
column sz = ColumnSpec sz Nothing Nothing

-- |Create a table widget using a list of column specifications and a
-- border style.
newTable :: [ColumnSpec]
         -> BorderStyle
         -> IO (Widget Table)
newTable specs borderSty = do
  let initSt = Table { rows = []
                     , columnSpecs = specs
                     , borderStyle = borderSty
                     , numColumns = length specs
                     , borderAttr = defAttr
                     , defaultCellAlignment = AlignLeft
                     , defaultCellPadding = padNone
                     }

  t <- newWidget initSt $ \w ->
      w { growHorizontal_ = \st -> do
            return $ any (== ColAuto) (map columnSize $ columnSpecs st)

        , render_ =
            \this sz ctx -> do
              rs <- rows <~~ this
              let sk = skin ctx

              rowImgs <- mapM (\(TableRow r) -> renderRow this sz r ctx) rs

              rowBorder <- mkRowBorder this sz ctx $ skinIntersectionFull sk
              topBorder <- mkTopBottomBorder this sz ctx $ skinIntersectionT sk
              bottomBorder <- mkTopBottomBorder this sz ctx $ skinIntersectionB sk
              sideBorderL <- mkSideBorder this ctx True
              sideBorderR <- mkSideBorder this ctx False

              let body = vertCat $ intersperse rowBorder rowImgs
                  withTBBorders = vertCat [topBorder, body, bottomBorder]
                  withSideBorders = horizCat [sideBorderL, withTBBorders, sideBorderR]

              -- Ideally, we would only display rows that we have room
              -- to render, but this is a much easier cop-out. :)
              if ((regionWidth sz < imageWidth withSideBorders) ||
                  (regionHeight sz < imageHeight withSideBorders)) then
                  return emptyImage else
                  return withSideBorders

        , setCurrentPosition_ =
            \this pos -> do
              sz <- getCurrentSize this
              if regionWidth sz == 0 || regionHeight sz == 0 then
                  return () else
                  do
                    bs <- borderStyle <~~ this
                    rs <- rows <~~ this

                    let edgeOffset = if edgeBorders bs
                                     then 1 else 0
                        positionRows _ [] = return ()
                        positionRows height ((TableRow row):rest) =
                          do
                            -- Compute the position for this row based on
                            -- border settings
                            let rowPos = pos `plusWidth` edgeOffset
                                         `withHeight` height

                            -- Get the maximum cell height
                            cellPhysSizes <- forM row $ \cell ->
                                             case cell of
                                               TableCell cw _ _ -> getCurrentSize cw
                                               EmptyCell -> return (0, 1)

                            -- Include 1 as a possible height to
                            -- prevent zero-height images from
                            -- breaking position computations.  This
                            -- won't hurt in the case where other
                            -- cells are bigger, since their heights
                            -- will be chosen instead.
                            let maxSize = maximum $ 1 : map regionHeight cellPhysSizes
                                borderOffset = if rowBorders bs
                                               then 1 else 0

                            -- Position the individual row widgets
                            -- (again, based on border settings)
                            positionRow this bs rowPos row
                            positionRows (height + maxSize + borderOffset) rest

                    positionRows (regionHeight pos + edgeOffset) rs
        }
  return t

getCellAlignment :: Widget Table -> Int -> TableCell -> IO Alignment
getCellAlignment _ _ (TableCell _ (Just p) _) = return p
getCellAlignment t columnNumber _ = do
  -- If the column for this cell has properties, use those; otherwise
  -- default to table-wide properties.
  specs <- columnSpecs <~~ t
  let spec = specs !! columnNumber

  case columnAlignment spec of
    Nothing -> defaultCellAlignment <~~ t
    Just p -> return p

getCellPadding :: Widget Table -> Int -> TableCell -> IO Padding
getCellPadding _ _ (TableCell _ _ (Just p)) = return p
getCellPadding t columnNumber _ = do
  -- If the column for this cell has properties, use those; otherwise
  -- default to table-wide properties.
  specs <- columnSpecs <~~ t
  let spec = specs !! columnNumber

  case columnPadding spec of
    Nothing -> defaultCellPadding <~~ t
    Just p -> return p

mkRowBorder :: Widget Table -> DisplayRegion -> RenderContext -> Char -> IO Image
mkRowBorder t sz ctx intChar = do
  bs <- borderStyle <~~ t

  if not $ rowBorders bs then
      return emptyImage else
      mkRowBorder_ t sz ctx intChar

-- Make a row border that matches the width of each row but does not
-- include outermost edge characters.
mkRowBorder_ :: Widget Table -> DisplayRegion -> RenderContext -> Char -> IO Image
mkRowBorder_ t sz ctx intChar = do
  bs <- borderStyle <~~ t
  bAttr <- borderAttr <~~ t
  specs <- columnSpecs <~~ t
  aw <- autoWidth t sz

  let sk = skin ctx
      bAttr' = mergeAttrs [ overrideAttr ctx
                          , bAttr
                          , normalAttr ctx
                          ]
      szs = map columnSize specs
      intersection = string bAttr' [intChar]
      imgs = (flip map) szs $ \s ->
             case s of
               ColFixed n -> charFill bAttr' (skinHorizontal sk) n 1
               ColAuto -> charFill bAttr' (skinHorizontal sk) aw 1
      imgs' = if colBorders bs
              then intersperse intersection imgs
              else imgs

  return $ horizCat imgs'

mkTopBottomBorder :: Widget Table -> DisplayRegion -> RenderContext -> Char -> IO Image
mkTopBottomBorder t sz ctx intChar = do
  bs <- borderStyle <~~ t

  if edgeBorders bs then
      mkRowBorder_ t sz ctx intChar else
      return emptyImage

-- Make vertical side borders for the table, including row border
-- intersections if necessary.
mkSideBorder :: Widget Table -> RenderContext -> Bool -> IO Image
mkSideBorder t ctx isLeft = do
  bs <- borderStyle <~~ t

  if edgeBorders bs then
      mkSideBorder_ t ctx isLeft else
      return emptyImage

mkSideBorder_ :: Widget Table -> RenderContext -> Bool -> IO Image
mkSideBorder_ t ctx isLeft = do
  bs <- borderStyle <~~ t
  bAttr <- borderAttr <~~ t
  rs <- rows <~~ t

  let sk = skin ctx
      intersection = string bAttr' [ if isLeft
                                     then skinIntersectionL sk
                                     else skinIntersectionR sk
                                   ]
      topCorner = string bAttr' [ if isLeft
                                  then skinCornerTL sk
                                  else skinCornerTR sk
                                ]
      bottomCorner = string bAttr' [ if isLeft
                                     then skinCornerBL sk
                                     else skinCornerBR sk
                                   ]
      bAttr' = mergeAttrs [ overrideAttr ctx
                          , bAttr
                          , normalAttr ctx
                          ]

  rowHeights <- forM rs $ \(TableRow row) -> do
                    hs <- forM row $ \cell ->
                          case cell of
                            TableCell cw _ _ -> regionHeight <$> getCurrentSize cw
                            EmptyCell -> return 1
                    return $ maximum hs

  let borderImgs = (flip map) rowHeights $ \h -> charFill bAttr' (skinVertical sk) 1 h
      withIntersections = if rowBorders bs
                          then intersperse intersection borderImgs
                          else borderImgs

  return $ vertCat $ topCorner : withIntersections ++ [bottomCorner]

positionRow :: Widget Table -> BorderStyle -> DisplayRegion -> [TableCell] -> IO ()
positionRow t bs pos cells = do
  -- Position each cell widget based on the base position of the row
  -- (which starts from the origin of the leftmost widget, NOT the
  -- leftmost cell border)
  oldSize <- getCurrentSize t
  aw <- autoWidth t oldSize
  specs <- columnSpecs <~~ t

  let szs = map columnSize specs
      offset = if colBorders bs
               then 1
               else 0

      cellWidth ColAuto = aw
      cellWidth (ColFixed n) = toEnum n

      doPositioning _ [] = return ()
      doPositioning width ((szPolicy, cell):ws) =
          do
            case cell of
              TableCell w _ _ -> setCurrentPosition w $ pos `plusWidth` width
              EmptyCell -> return ()
            doPositioning (width + cellWidth szPolicy + offset) ws

  doPositioning 0 $ zip szs cells

autoWidth :: Widget Table -> DisplayRegion -> IO Int
autoWidth t sz = do
  specs <- columnSpecs <~~ t
  bs <- borderStyle <~~ t

  let sizes = map columnSize specs
      numAuto = length $ filter (== ColAuto) sizes
      totalFixed = sum $ (flip map) sizes $ \s ->
                   case s of
                     ColAuto -> 0
                     ColFixed n -> n
      edgeWidth = if edgeBorders bs then 2 else 0
      colWidth = if colBorders bs then (toEnum $ length sizes - 1) else 0

  return $ toEnum ((max 0 ((fromEnum $ regionWidth sz) - totalFixed - edgeWidth - colWidth))
                   `div` numAuto)

-- |Add a heading row to a table.  Adds a row using the specified
-- |labels and attribute.  Returns the widgets it constructed as a
-- |side-effect in case you want to do something with them.
addHeadingRow :: Widget Table -> Attr -> [T.Text] -> IO [Widget FormattedText]
addHeadingRow tbl attr labels = do
  ws <- mapM (\s -> plainText s >>= withNormalAttribute attr) labels
  addRow tbl ws
  return ws

-- |Add a heading row to a table.  Adds a row using the specified
-- |labels and attribute.
addHeadingRow_ :: Widget Table -> Attr -> [T.Text] -> IO ()
addHeadingRow_ tbl attr labels = addHeadingRow tbl attr labels >> return ()

applyCellAlignment :: Alignment -> TableCell -> IO TableCell
applyCellAlignment _ EmptyCell = return EmptyCell
applyCellAlignment al (TableCell w a p) = do
  case al of
    AlignLeft -> return $ TableCell w a p

    AlignCenter -> do
      -- This check really belongs in the centering code...
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
                  w' <- (hFill ' ' 1) <++> (return w)
                  return $ TableCell w' a p
        True -> return $ TableCell w a p

applyCellPadding :: Padding -> TableCell -> IO TableCell
applyCellPadding _ EmptyCell = return EmptyCell
applyCellPadding padding (TableCell w a p) = do
  w' <- padded w padding
  return $ TableCell w' a p

-- |Add a row to the table.  Use 'RowLike' instances to populate the
-- row.  Throws 'BadTableWidgetSizePolicy' if any widgets in the row
-- grow vertically; throws 'ColumnCountMismatch' if the row's number
-- of columns does not match the table's column count.
addRow :: (RowLike a) => Widget Table -> a -> IO ()
addRow t row = do
  let (TableRow cells_) = mkRow row

  cells <- forM (zip [1..] cells_) $ \(i, c) -> do
                 case c of
                   EmptyCell -> return ()
                   TableCell w _ _ -> do
                          v <- growVertical w
                          when (v) $ throw $ BadTableWidgetSizePolicy i

                 -- Apply cell properties to the widget in this cell.
                 alignment <- getCellAlignment t (i - 1) c
                 padding <- getCellPadding t (i - 1) c

                 applyCellAlignment alignment c >>= applyCellPadding padding

  nc <- numColumns <~~ t
  when (length cells /= nc) $ throw ColumnCountMismatch

  updateWidgetState t $ \s ->
      s { rows = rows s ++ [TableRow cells] }

renderCell :: DisplayRegion -> TableCell -> RenderContext -> IO Image
renderCell region EmptyCell ctx = do
  w <- plainText T.empty
  render w region ctx
renderCell region (TableCell w _ _) ctx =
    render w region ctx

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

rowHeight :: [Image] -> Int
rowHeight = maximum . map imageHeight

renderRow :: Widget Table -> DisplayRegion -> [TableCell] -> RenderContext -> IO Image
renderRow tbl sz cells ctx = do
  specs <- columnSpecs <~~ tbl
  borderSty <- borderStyle <~~ tbl
  bAttr <- borderAttr <~~ tbl
  aw <- autoWidth tbl sz

  let sk = skin ctx
      sizes = map columnSize specs
      att = mergeAttrs [ overrideAttr ctx
                       , normalAttr ctx
                       ]
      newDefault = normalAttr ctx

  cellImgs <-
      forM (zip cells sizes) $ \(cellW, sizeSpec) ->
          do
            let cellSz = (cellWidth, regionHeight sz)
                cellWidth = case sizeSpec of
                              ColFixed n -> toEnum n
                              ColAuto -> aw

            img <- renderCell cellSz cellW $ ctx { normalAttr = newDefault }
            -- Right-pad the image if it isn't big enough to fill the
            -- cell.
            case compare (imageWidth img) (regionWidth cellSz) of
              EQ -> return img
              LT -> return $ img <|> charFill att ' '
                       (max 0 (regionWidth cellSz - imageWidth img))
                       (max (imageHeight img) 1)
              GT -> throw CellImageTooBig

  let maxHeight = rowHeight cellImgs
      cellImgsBottomPadded = (flip map) cellImgs $ \img ->
                             img <-> charFill att ' ' (imageWidth img) (maxHeight - imageHeight img)

  -- If we need to draw borders in between columns, do that.
  let bAttr' = mergeAttrs [ overrideAttr ctx
                          , bAttr
                          , normalAttr ctx
                          ]
      withBorders = case colBorders borderSty of
                      False -> cellImgsBottomPadded
                      True -> intersperse (charFill bAttr' (skinVertical sk) 1 maxHeight) cellImgsBottomPadded

  return $ horizCat withBorders
