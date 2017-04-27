module Tetriminos where

import Graphics.Gloss
import Data.List
import System.Random -- get a random number for getting random Tetriminos

cellSize :: Int
cellSize = 35

data Tetriminos = TetriCoord [(Int,Int)] Color deriving(Show)
--tetriminos definitions
--use small case for value constructor
tetriminoO = TetriCoord [(-1,-1), (-1,1), (1,1),(1,-1)] (yellow)
tetriminoZ = TetriCoord [(-3,1), (-1,1), (-1,-1), (1,-1)] (cyan)
tetriminoS = TetriCoord [(3,1), (1,1), (1,-1), (-1,-1)] (orange)
tetriminoI = TetriCoord [(-3,1), (-1,-1), (1,-1), (3,-1)] (green)
tetriminoL = TetriCoord [(-1,3),(-1,1),(-1,-1),(1,-1)] (blue)
tetriminoJ = TetriCoord [(-1,-1),(1,-1),(1,1),(1,3)] (rose)
tetriminoT = TetriCoord [(-3,-1), (-1,-1), (-1,1), (1,-1)] (red)

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

drawTetri :: Tetriminos -> Picture
drawTetri (TetriCoord xs col) = pictures $ drawCells (map (cordToScreen) xs) col
  where
    drawCells :: [(Int,Int)] -> Color -> [Picture]
    drawCells [] _ = []
    drawCells (x : xs) col = (drawCell x col) : (drawCells xs col)

randomTetri :: Double -> Tetriminos
randomTetri r = case ((truncate (r * 1000)) `mod` 7) of
    0 -> tetriminoO
    1 -> tetriminoZ
    2 -> tetriminoS
    3 -> tetriminoI
    4 -> tetriminoL
    5 -> tetriminoJ
    6 -> tetriminoT