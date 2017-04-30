module Main where

import Graphics.Gloss
import Tetrominos
import Field
import State
import Render
import Movement
import System.Random
window :: Display
window = InWindow "Tetris_Mark" (1280,768) (200,200)

background :: Color
background = black

fps = 60

main :: IO()
main = do
  g <- newStdGen
  play window background fps initialState render userControl updateState
