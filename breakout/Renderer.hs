--Renderer that render the picture of breakout game

module Renderer where

import State
import Brick
import Data.Monoid
import Graphics.Gloss

renderBrick :: Brick-> Picture
renderBrick brick@(Brick{exist = False}) = blank
renderBrick brick@(Brick{coord =(x,y)
                         ,color_ = color1}) = translate (fromIntegral bx) (fromIntegral by) (color color1 (rectangleSolid bw bh))
    where
      bx = fst transformed
      by = snd transformed
      bw = 0.9 * (fromIntegral brickWidth)
      bh = 0.9 * (fromIntegral brickHeight)
      transformed = coordToScreen (x,y)

coordToScreen :: (Int,Int) -> (Int,Int)
coordToScreen (cx,cy) = (bx,by)
  where
    bx = -ceiling (fieldWidthF/2) + (brickWidth `quot` 2) + (cx-1)*brickWidth
    by = -(brickHeight `quot` 2 + brickHeight * (cy-1))

renderAll ::[Brick] -> [Picture]
renderAll [] = []
renderAll (x:xs) = [renderBrick x] ++ renderRest xs
   where
     renderRest :: [Brick] -> [Picture]
     renderRest [] = []
     renderRest (x:xs) = [renderBrick x] ++ renderRest xs
-- the index of brick starts from (1,1) to (10,4)
render :: World -> Picture
render w@(World { paddle_loc = paddle
              , ball_loc    = (ball_x, ball_y)
              , w_brickList = brickList_s})
  = translate 0 (fieldHeightF/2) $
    (translate paddle (-fieldHeightF + paddleHeight/2) $
     color blue $
     polygon paddlePath) <>
    (translate ball_x ball_y $
     color red $
     circleSolid ballRadius)<>
     (pictures $ renderAll brickList_s)<>
     scoreSystem
        where
          scoreSystem = translate (-200) (-300) (scale 0.2 0.2 (pictures [playerScore]))
	     where
	       playerScore = color (dark cyan) (Text finalScore)
	       finalScore = "Score: " ++ (show (totalScore w))
     
