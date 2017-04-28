module Main where

import Graphics.Gloss.Data.ViewPort -- only for testing
import Graphics.Gloss
import Tetrominos
import Field
import State
import Render
import System.Random
window :: Display
window = InWindow "Tetris_Mark" (1280,768) (200,200)

background :: Color
background = black

fps = 60

velocity :: Float
velocity = 6
--add another situations if we have accelaration
-- changing difficulty by changing stoppingTime
stoppingTime ::  Float
stoppingTime = 1.0/velocity

updateState :: Float -> State -> State
updateState t state = myUpdateState (state {time = t})

myUpdateState :: State -> State
myUpdateState state
  | timeToNextMove = movement state {stopTime = stoppingTime} --reset the stopTime
  | otherwise  = updatedStateTime
    where
     updatedStateTime = state {stopTime = (stopTime state) - (time state)}
     
     timeToNextMove :: Bool
     timeToNextMove = stopTime updatedStateTime <= 0

--test whether a position is inside or outside of the field area
validPos :: (Int,Int) -> Tetrominos -> Bool
validPos coord (TetriCoord cs col) = or $ map (checkElem coord) cs
   where
    checkElem :: (Int,Int) -> (Int,Int) -> Bool
    checkElem (a,b) (c,d)
     | (a + c) < -9 || (a + c) > 9 = False
     | (b + d) > 1 || (b + d) < -41 = False
     | otherwise = True 
 
movement :: State -> State
movement state
  | nextPosInvalid = fix state
  | otherwise = state {tetroPos = new_tetroPos}
     where
       nextPosInvalid = not (validPos (tetroPos state) (tetrominos state))
       new_tetroPos = (fst (tetroPos state), snd (tetroPos state) -2)
-- fix the falling piece to its current location and resets a new piece to state
fix :: State -> State
fix state = state
            { field = renderTetrominos (tetrominos state) (tetroPos state) (field state)
            , tetrominos = randomTetro (fst randomNum) --randomize new tetro
            , tetroPos = (0,0)
            , randomSeed = snd randomNum
            }
               where
                 randomNum :: (Double, StdGen)
                 randomNum = randomR (1.0, 1000.0) (randomSeed state)

{-
--helper for moveTetro
toInt :: Float -> Int
toInt x = round x
--I failed when I directly use velocity times time to move the tetro, it suddenly disappears.
--Then I figured out that I actually can use change my mind, I can let tetro stops at each row, test whether they are valid to pass, then move to the next row.
--the "speed" is actually the stopping time in these rows.

moveTetro :: Float -> State -> State
moveTetro seconds state = state {tetroPos = (x,y')}
  where
    -- Old locations and velocities
    (x,y) = tetroPos state
    --new locations
    -- x' = x + (fromIntegral velocity * seconds)
    y' = y + (toInt $ velocity * seconds)
--testState :: State
--testState = initialState {field = (testField emptyField)}
-}
main :: IO()
main = do
  g <- newStdGen
  simulate window background fps initialState render moving
    where
       moving :: ViewPort -> Float -> State -> State
       moving _ = updateState
