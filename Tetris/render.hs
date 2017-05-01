--render the picture each time
module Render where

import State
import Field
import Graphics.Gloss

-- the size of the standard cell in field
cellSize :: Int
cellSize = 35

--setting for field and wall
fieldWidth = 10 * cellSize
fieldHeight = 20 * cellSize -- two lines are invisible on screen, which are covered by the top wall
padding = (768 - fieldHeight) `quot` 2
wallWidth = fieldWidth + 2 * padding
wallHeight = fieldHeight + 2 * padding

--Color of wall and field
fieldColor = black
wallColor = greyN 0.5
 
--change the coordinate to normal position on Screen
cordToScreen :: (Int,Int) -> (Int,Int)
cordToScreen (fx,fy) = (rx,ry)
  where
     rx = (fx * cellSize) `quot` 2
     --not able to use /, since it will have period and number less than 1, which is no good for screen representing
     ry = (11 * cellSize) + (fy * cellSize) `quot` 2 -- fy is the line number,which if it be increase by 2, we will add one cellSize in Screen

-- take the coordinate of a cell, and then draw it on screen
drawCell :: (Int,Int) -> Color -> Picture
drawCell (fx,fy) col = translate (fromIntegral sx) (fromIntegral sy) $ (color col $ rectangleSolid sz sz)
  where
    sx = fst $ cordToScreen (fx,fy)
    sy = snd $ cordToScreen (fx,fy)
    sz = 0.9 * (fromIntegral cellSize)

drawField :: Field -> Picture
drawField f = pictures (map cellToScreen (coordCells f))
    where
      cellToScreen (fx,fy,c)
         | fy > -3 = pictures[] --if the tetrominos is in the top-wall, do not show it. It is weird
         | c == Empty = pictures[] -- let it be background color if it is an empty cell
         | otherwise = drawCell (fx,fy) (cellColor c)

render :: State -> Picture
render state = pictures [walls, playfield,activeTetro,scoreCount]
  where
    walls = color wallColor (rectangleSolid (fromIntegral wallWidth) (fromIntegral wallHeight))
    playfield = pictures [(color fieldColor (rectangleSolid (fromIntegral fieldWidth) (fromIntegral fieldHeight))), drawField (field state)]
    activeTetro = drawField (renderTetrominos (tetrominos state) (tetroPos state) emptyField)
    scoreCount = translate (-600.0) (200.0) (pictures [playerScore])
         where
           playerScore = color (greyN 0.5) $ Text $ "Score: " ++ (show(score state))


