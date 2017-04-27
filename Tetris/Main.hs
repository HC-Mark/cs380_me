module Main where

import Graphics.Gloss
import Tetriminos
import System.Random
window :: Display
window = InWindow "Tetris_Mark" (1280,768) (200,200)

background :: Color
background = black

fps = 60

main :: IO()
main = do
  g <- newStdGen
  let randomNum = head (randoms g :: [Double])
  display window background (drawTetri $ randomTetri randomNum)