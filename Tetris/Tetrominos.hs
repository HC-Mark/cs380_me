module Tetrominos where

import Graphics.Gloss
import System.Random -- get a random number for getting random Tetriminos

cellSize :: Int
cellSize = 35

data Tetrominos = TetriCoord [(Int,Int)] Color deriving(Show)
--tetriminos definitions
--use small case for value constructor
tetrominoO = TetriCoord [(-1,-1), (-1,1), (1,1),(1,-1)] (yellow)
tetrominoZ = TetriCoord [(-3,1), (-1,1), (-1,-1), (1,-1)] (cyan)
tetrominoS = TetriCoord [(3,1), (1,1), (1,-1), (-1,-1)] (orange)
tetrominoI = TetriCoord [(-3,-1), (-1,-1), (1,-1), (3,-1)] (green)
tetrominoL = TetriCoord [(-1,3),(-1,1),(-1,-1),(1,-1)] (blue)
tetrominoJ = TetriCoord [(-1,-1),(1,-1),(1,1),(1,3)] (rose)
tetrominoT = TetriCoord [(-3,-1), (-1,-1), (-1,1), (1,-1)] (red)

--whether that this tetriminos contains this specific coordinate
--this function will be used to check the state of field
containTetro :: (Int,Int) -> Tetrominos -> Bool
containTetro cord (TetriCoord xs _) = elem cord xs

{-
cordToScreen :: (Int,Int) -> (Int,Int)
cordToScreen (px,py) = (rx,ry)
  where
     rx = (px * cellSize) `quot` 2
     --not able to use /, since it will have period and number less than 1, which is no good for screen representing
     ry = (py * cellSize) `quot` 2


drawCell :: (Int,Int) -> Color -> Picture
drawCell (rx,ry) col = translate (fromIntegral rx) (fromIntegral ry) $ (color col $ rectangleSolid sz sz)
  where
    sz = 0.9 * (fromIntegral cellSize)

drawTetri :: Tetrominos -> Picture
drawTetri (TetriCoord xs col) = pictures $ drawCells (map (cordToScreen) xs) col
  where
    drawCells :: [(Int,Int)] -> Color -> [Picture]
    drawCells [] _ = []
    drawCells (x : xs) col = (drawCell x col) : (drawCells xs col)
-}
randomTetro :: Double -> Tetrominos
randomTetro r = case ((truncate r) `mod` 7) of
    0 -> tetrominoO
    1 -> tetrominoZ
    2 -> tetrominoS
    3 -> tetrominoI
    4 -> tetrominoL
    5 -> tetrominoJ
    6 -> tetrominoT

tetroColor :: Tetrominos -> Color
tetroColor (TetriCoord _ col) = col
