{-# LANGUAGE TypeApplications #-}
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
                   , w_playing :: Bool
                   , w_brickList :: [Brick]}

initialWorld = World { w_lpaddle = 0
                     , w_ball    = (0,-stageHeightF/2)
                     , w_ball_motion = (ballSpeed, 0)
                     , w_paddle_motion  = 0
                     , w_playing = False
                     , w_brickList = brickList}

render :: World -> Picture
render (World { w_lpaddle = lpaddle
              , w_ball    = (ball_x, ball_y)
              , w_brickList = brickList_s})
  = translate 0 (stageHeightF/2) $
    (translate lpaddle (-stageHeightF + paddleHeight/2) $
     color blue $
     polygon paddlePath) <>
    (translate ball_x ball_y $
     color red $
     circleSolid ballRadius)<>
     (pictures $ renderAll brickList_s)
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
                , w_paddle_motion = paddle_motion
                , w_brickList = brickList_s})
  = let lpaddle' = clampPaddle (lpaddle + paddle_motion)
        ball_x'  = ball_x + ball_dx
        ball_y'  = ball_y + ball_dy

        ball_dx'
          | ball_x' < -stageWidthF/2 + ballRadius
          = ballSpeed

          | ball_x' > stageWidthF/2 - ballRadius
          = -ballSpeed
-- should not directly put this conditon here, since we want the ball to move in opposite direction only when it touch the brick
-- hence, it's y should also be concern
-- horizontal collides means change the 

          | ballVertCollides ball_y' brickList_s
          = -ball_dx

          | otherwise
          = ball_dx

        ball_dy'
          | ball_y' < -stageHeightF + paddleHeight + ballRadius
          , ball_x' `inRange` (lpaddle' - paddleWidth/2, lpaddle' + paddleWidth/2)
          = ballSpeed

          | ball_y' > -(ballRadius + paddleHeight)
          = -ballSpeed

          | ballHoriCollides ball_x' ball_y' brickList_s
          = -ball_dy          

          | otherwise
          = ball_dy

        brickList_s' =  brickExistTest ball_x' ball_y' brickList_s  

        playing' =  not (ball_y' < -(stageHeightF-ballRadius))
    in
    w { w_lpaddle = lpaddle'
      , w_ball    = (ball_x', ball_y')
      , w_ball_motion = (ball_dx',ball_dy')
      , w_playing = playing'	   
      , w_brickList = brickList_s'}

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
data Brick = Brick {
               coord :: (Int,Int)
             , color_ :: Color
             , exist :: Bool
             }deriving (Show)

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

-- handy to use when we need to divide
brickWidthF :: Float
brickWidthF = fromIntegral brickWidth
brickHeightF :: Float
brickHeightF = fromIntegral brickHeight
row = 4
col = 10

-- where should I call this function to create this list?
-- find a way to make it a constant list

brickList :: [Brick]
brickList = createBrickList col row
  where 
   createBrickList :: Int -> Int -> [Brick]
   createBrickList _ 0 = []
   createBrickList 0 row = createBrickList col (row-1)
   createBrickList col row = [createBrick(col,row)] ++ (createBrickList (col-1) row)
 {-  
brickList :: Int -> Int ->[Brick]
brickList _ 0 = []
brickList 0 row = brickList col (row-1)
brickList col row = [createBrick(col,row)] ++ (brickList (col-1) row)
-}
createBrick :: (Int,Int) -> Brick
createBrick (x,y) = Brick{
                     coord = (x,y)
                     , color_ = getColor (x+y)
                     , exist = True
                    }

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
    bx = -ceiling (stageWidthF/2) + (brickWidth `quot` 2) + (cx-1)*brickWidth
    by = -(brickHeight `quot` 2 + brickHeight * (cy-1))

renderAll ::[Brick] -> [Picture]
renderAll [] = []
renderAll (x:xs) = [renderBrick x] ++ renderRest xs
   where
     renderRest :: [Brick] -> [Picture]
     renderRest [] = []
     renderRest (x:xs) = [renderBrick x] ++ renderRest xs
     
{-old method
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
-}

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

ballVertCollides :: Float -> [Brick] -> Bool
ballVertCollides ball_y brickList = testVert ball_y brickList
  where
    testVert :: Float -> [Brick] -> Bool
    testVert y [] = False
    testVert y (b@(Brick{exist = False}):bs) = False || testVert y bs
    testVert y (b@(Brick{coord = (bx,by)}):bs) = y `inRange` ((fromIntegral center_y) - brickHeightF/2 - ballRadius, (fromIntegral center_y) + brickHeightF/2+ballRadius) || (testVert y bs)
      where
       center_y = snd $ coordToScreen (bx,by)
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
main :: IO ()
main = play (InWindow "Pong" (stageWidth, stageHeight) (200, 200))
            white
            50
            initialWorld
            render
            react
            step
