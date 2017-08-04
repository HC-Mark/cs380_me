--this module helps us handle the imported event and control the existence of brick and collision of ball

module Event where

import Brick
import State
import Renderer
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- this function comes from Richard's Pong.hs, I use this to prevent the paddle going out of window
clampPaddle :: Float -> Float
clampPaddle pad = min (fieldWidthF/2 - paddleWidth / 2) $
                  max (-fieldWidthF/2 + paddleWidth/2) $
                  pad


event :: Float -> World -> World
event _ w@(World { playing = False }) = w
event _ w@(World { paddle_loc = paddle
                , ball_loc    = (ball_x, ball_y)
                , ball_motion = (ball_dx, ball_dy)
                , paddle_motion = paddle_motion
                , w_brickList = brickList_s
                , totalScore = score})
  = let paddle' = clampPaddle (paddle + paddle_motion)
        ball_x'  = ball_x + ball_dx
        ball_y'  = ball_y + ball_dy

        ball_dx'
          | ball_x' < -fieldWidthF/2 + ballRadius
          = ballSpeed

          | ball_x' > fieldWidthF/2 - ballRadius
          = -ballSpeed
-- should not directly put this conditon here, since we want the ball to move in opposite direction only when it touch the brick
-- hence, it's y should also be concern
-- horizontal collides means change the 

          | ballVertCollides ball_x' ball_y' brickList_s
          = -ball_dx

          | otherwise
          = ball_dx

        ball_dy'
          | ball_y' < -fieldHeightF + paddleHeight + ballRadius
          , ball_x' `inRange` (paddle' - paddleWidth/2, paddle' + paddleWidth/2)
          = ballSpeed

          | ball_y' > -(ballRadius + paddleHeight)
          = -ballSpeed

          | ballHoriCollides ball_x' ball_y' brickList_s
          = -ball_dy          

          | otherwise
          = ball_dy

        brickList_s' = brickExistTest ball_x' ball_y' brickList_s  

        score'
          |ballHoriCollides ball_x' ball_y' brickList_s = score + 1
          |ballVertCollides ball_x' ball_y' brickList_s = score + 1
          |otherwise = score

        playing' =  not (ball_y' < -(fieldHeightF-ballRadius))
    in
    w { paddle_loc = paddle'
      , ball_loc    = (ball_x', ball_y')
      , ball_motion = (ball_dx',ball_dy')
      , playing = playing'
      , w_brickList = brickList_s'
      , totalScore= score'}

react :: Event -> World -> World
react (EventKey (SpecialKey KeyEnter) Down _ _)
      w@(World { playing = False })
  = w { playing = True
      , ball_loc    = (0, -fieldHeightF / 2)
      , ball_motion = (ballSpeed, ballSpeed)
      , paddle_motion = 0 }    
react ev w@(World { playing = True })
  | EventKey (SpecialKey KeyLeft) Down _ _ <- ev
  = w { paddle_motion = -(2 * ballSpeed) }
  | EventKey (SpecialKey KeyRight) Down _ _ <- ev
  = w { paddle_motion = 2 * ballSpeed }
  | EventKey _ Up _ _ <- ev
  = w { paddle_motion = 0 }
react _ w = w


ballHoriCollides :: Float -> Float -> [Brick] -> Bool
ballHoriCollides ball_x ball_y brickList = testHori ball_x ball_y brickList
   where
     testHori :: Float -> Float -> [Brick] -> Bool
     testHori x _ [] = False
     testHori x y  (b@(Brick{exist = False}):bs) = False || testHori x y bs
     testHori x y (b@(Brick{coord = (bx,by)}):bs) = (x `inRange` ((fromIntegral center_x) - brickWidthF/2 - ballRadius , (fromIntegral center_x) + brickWidthF/2 +ballRadius ) && y `inRange`((fromIntegral center_y) - brickHeightF/2- ballRadius , (fromIntegral center_y) + brickHeightF/2 + ballRadius)) || (testHori x y bs)
     -- I am considering to add a ballRadius here, but it shows that it will always make the ball goes to opposite x direction
       where
         center_x = fst $ coordToScreen (bx,by)
         center_y = snd $ coordToScreen (bx,by)
     
-- error "ballHoriCollides : unimplemented"

ballVertCollides :: Float -> Float -> [Brick] -> Bool
ballVertCollides ball_x ball_y brickList = testVert ball_x ball_y brickList
  where
    testVert :: Float -> Float -> [Brick] -> Bool
    testVert _ _ [] = False
    testVert x y (b@(Brick{exist = False}):bs) = False || testVert x y bs
    testVert x y (b@(Brick{coord = (bx,by)}):bs) =(y `inRange` ((fromIntegral center_y) - brickHeightF/2 - ballRadius, (fromIntegral center_y) + brickHeightF/2+ballRadius) &&  x `inRange` ((fromIntegral center_x) - brickWidthF/2 - ballRadius , (fromIntegral center_x) + brickWidthF/2 +ballRadius )) || (testVert x y bs)
      where
       center_y = snd $ coordToScreen (bx,by)
       center_x = fst $ coordToScreen (bx,by)
-- error "ballVertCollides : unimplemented"

--test whether we touch a brick, if so, we turn that brick to false, which will not be shown
brickExistTest :: Float -> Float -> [Brick] -> [Brick]
brickExistTest _ _ [] = []
brickExistTest ball_x ball_y (b@(Brick{exist = False}):bs) = b: (brickExistTest ball_x ball_y bs)
brickExistTest ball_x ball_y (b@(Brick{coord = (bx,by), exist = True}) :bs)
   | ball_x `inRange` ((fromIntegral center_x) - brickWidthF/2 - ballRadius , (fromIntegral center_x) + brickWidthF/2 +ballRadius) && ball_y `inRange` ((fromIntegral center_y) - brickHeightF/2- ballRadius , (fromIntegral center_y) + brickHeightF/2 + ballRadius) = (removeBrick b) : bs
   | otherwise = b : (brickExistTest ball_x ball_y bs)
     where
       removeBrick :: Brick -> Brick
       removeBrick (Brick {coord = (bx,by), color_ = col, exist = True}) = Brick {coord = (bx,by), color_ = col, exist = False}
       center_x = fst $ coordToScreen (bx,by)
       center_y = snd $ coordToScreen (bx,by)
