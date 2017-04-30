--the game's state

module State where

import Field
import Tetrominos
import System.Random

data State = Create
             { field :: Field
             , time :: Float
             , stopTime :: Float
             , tetrominos :: Tetrominos
             , tetroPos :: (Int,Int)
             , randomSeed :: StdGen
             , speedUp :: Bool
             } deriving (Show)

initialState :: State
initialState = Create
               { field = emptyField
               , time = 0.0
               , stopTime = 0.0
               , tetrominos = tetrominoI
               , tetroPos = (0,0)
               , randomSeed = mkStdGen 0 -- create a new random generator
               , speedUp = False -- the tetro should have normal speed at first
               }

resetState :: State -> State
resetState s = initialState {randomSeed = (randomSeed s)}
--without resetting the randomSeed will make the output of tetrominos be the same everytime
