module Movement where

import Graphics.Gloss
import State
import Tetrominos
import Field
import Graphics.Gloss.Interface.Pure.Game -- for handling event
import System.Random

velocity :: Float
velocity = 6
--add another situations if we have accelaration
-- changing difficulty by changing stoppingTime
speedUpV :: Float
speedUpV = 30

actualSpeed :: State -> Float
actualSpeed state
  |speedUp state = speedUpV
  |otherwise = velocity

stoppingTime ::  State -> Float
stoppingTime state = 1.0 / (actualSpeed state)

userControl :: Event -> State -> State
userControl (EventKey (SpecialKey KeyLeft) Down _ _) state = moveTetro (-2) state
userControl (EventKey (SpecialKey KeyRight) Down _ _) state = moveTetro 2 state
userControl (EventKey (SpecialKey KeyDown) Down _ _) state = state {speedUp = True}
userControl (EventKey (SpecialKey KeyDown) Up _ _ ) state = state {speedUp = False}
userControl _ state = state --without this line, the program will crash

moveTetro :: Int -> State -> State
moveTetro steps state
  |tetroValidPos new_tetroPos (tetrominos state) (field state) = state {tetroPos = new_tetroPos}
  |otherwise = state
     where
      new_tetroPos = (fst(tetroPos state) + steps, snd(tetroPos state))

updateState :: Float -> State -> State
updateState t state = myUpdateState (state {time = t})

myUpdateState :: State -> State
myUpdateState state
  | timeToNextMove = movement state {stopTime = (stoppingTime state)} --reset the stopTime
  | otherwise  = updatedStateTime
    where
     updatedStateTime = state {stopTime = (stopTime state) - (time state)}
     
     timeToNextMove :: Bool
     timeToNextMove = stopTime updatedStateTime <= 0

--test whether a position is inside or outside of the field area
fieldValidPos :: (Int,Int) -> Tetrominos -> Bool
fieldValidPos coord (TetriCoord cs col) = and $ map (checkElem coord) cs
   where
    checkElem :: (Int,Int) -> (Int,Int) -> Bool
    checkElem (a,b) (c,d)
     | (a + c) < -9 || (a + c) > 9 = False
     | (b + d) > 1 || (b + d) < -41 = False
     | otherwise = True 

tetroValidPos :: (Int,Int) -> Tetrominos -> Field -> Bool
tetroValidPos coord tetro field = (fieldValidPos coord tetro) && not (tetroCollides tetro coord field)

movement :: State -> State
movement state
  | nextPosInvalid = handleFullRows $ fix state
  | otherwise = state {tetroPos = new_tetroPos}
     where
       nextPosInvalid = not (tetroValidPos new_tetroPos (tetrominos state) (field state))
       new_tetroPos = (fst (tetroPos state), snd (tetroPos state) -2)
-- fix the falling piece to its current location and resets a new piece to state
fix :: State -> State
fix state 
  | (snd (tetroPos state)) >= -1 = resetState state -- restart the game when one tetro overflows the top wall
  | otherwise = state
                    { field = renderTetrominos (tetrominos state) (tetroPos state) (field state)
                    , tetrominos = randomTetro (fst randomNum) --randomize new tetro
                    , tetroPos = (0,0)
                    , randomSeed = snd randomNum
                    , speedUp = False
                    }
                      where
                        randomNum :: (Double, StdGen)
                        randomNum = randomR (1.0, 1000.0) (randomSeed state)


clearCountLines :: Field -> (Field, Int)
clearCountLines (Rows rs) = (new_field, count)
   where
     -- add the cleared line(empty row) to the top of screen
     new_field = Rows (cleared ++ remaining)
     remaining = filter notFull rs
     count = length (filter isFull rs)
     cleared :: [Row]
     cleared = replicate count emptyRow
     
     isFull :: Row -> Bool
     isFull (Cells cs)  = and (map filled cs)

     notFull :: Row -> Bool
     notFull r = not (isFull r)

     filled :: Cell -> Bool
     filled c
         | c /= Empty = True
         | otherwise = False

handleFullRows :: State -> State
handleFullRows state = state {
                              field = fst result
                             , score = (score state) + 10 * (snd result) -- get ten points for each full row
                             }
          where
            result = clearCountLines $ field state


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
