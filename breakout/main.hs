module Pong where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

paddleWidth, paddleHeight :: Float
paddleWidth = 40
paddleHeight = 10

paddlePath :: Path
paddlePath = [ (-paddleWidth / 2, paddleHeight / 2)
             , (-paddleWidth / 2, -paddleHeight / 2)
             , (paddleWidth / 2, -paddleHeight / 2)
             , (paddleWidth / 2, paddleHeight / 2) ]

ballRadius :: Float
ballRadius = 8

ballSpeed :: Float
ballSpeed = 2

stageWidth, stageHeight :: Int
stageWidth = 600
stageHeight = 400

stageWidthF, stageHeightF :: Float
stageWidthF = fromIntegral stageWidth
stageHeightF = fromIntegral stageHeight
--now we want paddle to be horizontal

data World = World { w_lpaddle :: Float
                   , w_ball    :: (Float, Float)
                   , w_ball_motion :: (Float, Float)
                   , w_paddle_motion  :: Float
                   , w_playing :: Bool }

initialWorld = World { w_lpaddle = 0
                     , w_ball    = (0,-stageHeightF/2)
                     , w_ball_motion = (ballSpeed, 0)
                     , w_paddle_motion  = 0
                     , w_playing = False }

render :: World -> Picture
render (World { w_lpaddle = lpaddle
              , w_ball    = (ball_x, ball_y) })
  = translate 0 (stageHeightF/2) $
    (translate lpaddle (-stageHeightF + paddleHeight/2) $
     color blue $
     polygon paddlePath) <>
    (translate ball_x ball_y $
     color red $
     circleSolid ballRadius) <>
     (pictures renderAll)
     --first brick has index (1,1)
-- here we first translate the origin to left top of screen, then we create two paddles and a ball

clampPaddle :: Float -> Float
clampPaddle pad = min (stageWidthF/2 - paddleWidth / 2) $
                  max (-stageWidthF/2 + paddleWidth/2) $
                  pad

inRange :: Ord a => a -> (a, a) -> Bool
x `inRange` (a, b) = x >= a && x <= b

-- without clampPaddle(lpaddle + paddle_motion) the paddle will go out of the screen

step :: Float -> World -> World
step _ w@(World { w_playing = False }) = w
step _ w@(World { w_lpaddle = lpaddle
                , w_ball    = (ball_x, ball_y)
                , w_ball_motion = (ball_dx, ball_dy)
                , w_paddle_motion = paddle_motion })
  = let lpaddle' = clampPaddle (lpaddle + paddle_motion)
        ball_x'  = ball_x + ball_dx
        ball_y'  = ball_y + ball_dy

        ball_dx'
          | ball_x' < -stageWidthF/2 + ballRadius
          = ballSpeed

          | ball_x' > stageWidthF/2 - ballRadius
          = -ballSpeed

          | otherwise
          = ball_dx

        ball_dy'
          | ball_y' < -stageHeightF + paddleHeight + ballRadius
          , ball_x' `inRange` (lpaddle' - paddleWidth/2, lpaddle' + paddleWidth/2)
          = ballSpeed

          | ball_y' > -(ballRadius + paddleHeight)
          = -ballSpeed

          | otherwise
          = ball_dy

        playing' =  not (ball_y' < -(stageHeightF-ballRadius))
    in
    w { w_lpaddle = lpaddle'
      , w_ball    = (ball_x', ball_y')
      , w_ball_motion = (ball_dx', ball_dy')
      , w_playing = playing' }

react :: Event -> World -> World
react (EventKey (MouseButton LeftButton) Down _ _)
      w@(World { w_playing = False })
  = w { w_playing = True
      , w_ball    = (0, -stageHeightF / 2)
      , w_ball_motion = (ballSpeed, ballSpeed)
      , w_paddle_motion = 0 }
react ev w@(World { w_playing = True })
  | EventKey (SpecialKey KeyLeft) Down _ _ <- ev
  = w { w_paddle_motion = -(2 * ballSpeed) }
  | EventKey (SpecialKey KeyRight) Down _ _ <- ev
  = w { w_paddle_motion = 2 * ballSpeed }
  | EventKey _ Up _ _ <- ev
  = w { w_paddle_motion = 0 }
react _ w = w

-- build up a system to store all bricks in the game
getColor :: Int -> Color
getColor r = case(r `mod` 5) of
    0 -> red
    1 -> (dark green)
    2 -> (blue)
    3 -> (dark orange)
    4 -> (dark magenta)

-- set up the size of brick
brickWidth = 60
-- stageWidthF / (fromIntegral col)
brickHeight = 20
row = 4
col = 10

renderBrick ::(Int,Int) -> Picture
renderBrick (a,b) = translate (fromIntegral bx) (fromIntegral by) (color col (rectangleSolid bw bh))
    where
      bx = fst transformed
      by = snd transformed
      bw = 0.9 * (fromIntegral brickWidth)
      bh = 0.9 * (fromIntegral brickHeight)
      col = getColor (a+b)
      transformed = coordToScreen (a,b)
    

coordToScreen :: (Int,Int) -> (Int,Int)
coordToScreen (cx,cy) = (bx,by)
  where
    bx = -ceiling (stageWidthF/2) + (brickWidth `quot` 2) + (cx-1)*brickWidth
    by = -(brickHeight `quot` 2 + brickHeight * (cy-1))

renderAll ::[Picture]
renderAll = (renderRow row)
  where
    renderRow :: Int -> [Picture]
    renderRow 0 = []
    renderRow y = (renderCol y col) <> renderRow (y-1)
    renderCol :: Int -> Int -> [Picture]
    renderCol _ 0 = []
    renderCol y x = [renderBrick(x,y)] <> (renderCol y (x-1))
    
main :: IO ()
main = play (InWindow "Pong" (stageWidth, stageHeight) (200, 200))
            white
            50
            initialWorld
            render
            react
            step