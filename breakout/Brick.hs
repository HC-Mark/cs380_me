--this module contains a helpful type Brick and related functions, also it contains the basic setting.

module Brick where

import Graphics.Gloss


paddleWidth, paddleHeight :: Float
paddleWidth = 60
paddleHeight = 10

paddlePath :: Path
paddlePath = [ (-paddleWidth / 2, paddleHeight / 2)
             , (-paddleWidth / 2, -paddleHeight / 2)
             , (paddleWidth / 2, -paddleHeight / 2)
             , (paddleWidth / 2, paddleHeight / 2) ] --from Richard's Pong.hs

ballRadius :: Float
ballRadius = 8

ballSpeed :: Float
ballSpeed = 2

fieldWidth, fieldHeight :: Int
fieldWidth = 600
fieldHeight = 400

fieldWidthF, fieldHeightF :: Float
fieldWidthF = fromIntegral fieldWidth
fieldHeightF = fromIntegral fieldHeight

inRange :: Ord a => a -> (a, a) -> Bool
x `inRange` (a, b) = x >= a && x <= b

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
brickWidth :: Int
brickWidth = 60
brickHeight :: Int
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