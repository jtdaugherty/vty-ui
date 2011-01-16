{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Vty.Widgets.Table
    ( Table
    , ColumnSize(..)
    , BorderStyle(..)
    , BorderFlag(..)
    , mkCell
    , newTable
    , addRow
    , addHeadingRow
    , addHeadingRow_
    )
where

import Data.Word
    ( Word
    )
import Data.List
    ( intersperse
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
    , updateWidgetState_
    , withWidth
    , setPhysicalPosition
    , getPhysicalSize
    )
import Graphics.Vty.Widgets.Text
    ( FormattedText
    , simpleText
    )

data TableCell = forall a. TableCell (Widget a)

data BorderFlag = Rows | Columns | Edges
                  deriving (Eq, Show)

data BorderStyle = BorderPartial [BorderFlag]
                 | BorderFull
                 | BorderNone
                   deriving (Eq, Show)

data ColumnSize = Fixed Int | Auto
                  deriving (Eq, Show)

mkCell :: Widget a -> TableCell
mkCell = TableCell

data Table = Table { rows :: [[TableCell]]
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

              rowImgs <- mapM (\r -> renderRow this sz r mAttr) rs

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
                  positionRows height (row:rest) =
                    do
                      -- Compute the position for this row based on
                      -- border settings
                      let rowPos = DisplayRegion (region_width pos + edgeOffset)
                                   height

                      -- Get the maximum cell height
                      cellPhysSizes <- forM row $ \(TableCell cw) -> getPhysicalSize cw
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

  rowHeights <- forM rs $ \row -> do
                    hs <- forM row $ \(TableCell cw) ->
                          getPhysicalSize cw >>= (return . region_height)
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
      doPositioning width ((szPolicy, TableCell w):ws) =
          do
            setPhysicalPosition w $ pos `withWidth` (region_width pos + width)
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
  addRow tbl $ map mkCell ws
  return ws

addHeadingRow_ :: (MonadIO m) => Widget Table -> Attr -> [String] -> m ()
addHeadingRow_ tbl attr labels = addHeadingRow tbl attr labels >> return ()

addRow :: (MonadIO m) => Widget Table -> [TableCell] -> m ()
addRow t cells = do
  nc <- numColumns <~~ t
  when (length cells /= nc) $
       error $ "New row column count (" ++ (show $ length cells) ++
                 ") does not match table column count (" ++
                 (show nc) ++ ")"

  updateWidgetState_ t $ \s ->
      s { rows = rows s ++ [cells] }

renderCell :: DisplayRegion -> TableCell -> Maybe Attr -> IO Image
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
              GT -> error "Table cell widget too big, should never happen!"

  -- If we need to draw borders in between columns, do that.
  let withBorders = case colBorders borderSty of
                      False -> cellImgs
                      True -> intersperse (char_fill bAttr '|' 1 (rowHeight cellImgs)) cellImgs

  return $ horiz_cat withBorders