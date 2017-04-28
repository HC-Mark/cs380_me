module Field where

import Tetrominos
import Data.List
import Graphics.Gloss
import Data.Maybe

--a cell is a rectangle in the field, it will either be filled with specific color or black(background color)
--how to do it by Maybe type?
data Cell = Empty | Filled Color deriving(Show,Eq)

--get color from a cell object, if it is Nothing(empty) then return black
cellColor :: Cell -> Color
cellColor Empty = black
cellColor (Filled col) = col

--create row of cells
data Row = Cells [Cell] deriving (Show)

--create an empty row
emptyRow :: Row
emptyRow = Cells (replicate 10 Empty)


data Field = Rows [Row] deriving (Show)

--create an empty Field
emptyField :: Field
emptyField = Rows( replicate 22 emptyRow)

--keep track the row number of each row in a field
-- each line number is the top line of each row
numberRows :: Field -> [(Int,Row)]
numberRows (Rows xs) = zip [1,(-1)..(-41)] xs --start at the top-mid of the screen

--keep track the cell number of each cell in a row
--each cell number is the left side of that cell
numberCells :: Row -> [(Int,Cell)]
numberCells (Cells cs) = zip [-9,-7..9] cs

--This function will give us the coordinate of this Cell in our field
--it will be very convenient for us to use this 3-tuple to render our field then just use field data type
coordCells :: Field -> [(Int,Int,Cell)]
coordCells f = concat (map getRow (numberRows f))
  where
    getRow (y,row) = map getCell (numberCells row)
        where
           getCell (x,c) = (x,y,c)

--helper function to get the third elements in coordCells
third :: (a,b,c) -> c
third (a,b,c) = c

--used for testing whether my Field is OK
testField :: Field -> Field
testField f = Rows (map changeRow (numberRows f))
  where
    changeRow :: (Int,Row) -> Row
    changeRow (y , cs)
      | y == -21 = Cells (map turnRed (numberCells cs))
      | otherwise = cs
        where
          turnRed :: (Int,Cell) -> Cell
          turnRed (_, c) =  Filled red

--check whether tetro is in odd coordinate -> important
--otherwise, if the initial position is on odd number, the tetro can not show correctly -- talke in presentation for the first situation
renderTetrominos :: Tetrominos -> (Int,Int) -> Field -> Field
renderTetrominos tetro (tx,ty) f =  Rows (map checkRow (numberRows f))
 -- | odd tx || odd ty = error "can not start at odd position, tetro can not show correctly! Please change a start point."
 -- | otherwise =  Rows (map checkRow (numberRows f))
    where
      checkRow :: (Int,Row) -> Row
      checkRow (y , rs) = Cells (map checkCell (numberCells rs))
       where
         checkCell :: (Int,Cell) -> Cell
         checkCell (x , c)
          | c /= Empty = c 
          | containTetro ((x - tx),(y - ty)) tetro = Filled (tetroColor tetro)
          | otherwise = Empty

-- here, I will use two field compared with each other,  the firest one is an emptyField with only that specific tetro in it. The second field is the previous field we have. And we compare each cell to see if there is cell filled by two tetros.
pieceCollides :: Tetrominos -> (Int,Int) -> Field -> Bool
pieceCollides tetro (tx,ty) old_f = checkField new_f old_f
   where
     new_f = renderTetrominos tetro (tx,ty) emptyField
     checkField :: Field -> Field -> Bool
     checkField (Rows cs1) (Rows cs2) = or $ checkRows cs1 cs2

     checkRows :: [Row] -> [Row] -> [Bool]
     checkRows [] [] = []
     checkRows (x@(Cells xs1) : xs) (y@(Cells xs2) : ys) = (or $ (checkCells xs1 xs2)) : checkRows xs ys

     checkCells :: [Cell] -> [Cell] -> [Bool]
     checkCells [] [] = []
     checkCells (x : xs) (y : ys) = (checkCell x y) : checkCells xs ys

     -- if neither x nor y is empty, then it must collide!
     checkCell :: Cell -> Cell -> Bool
     checkCell x y = (x /= Empty) && (y /= Empty)
