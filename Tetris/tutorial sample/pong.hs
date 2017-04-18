{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators, ScopedTypeVariables,UndecidableInstances,
             TypeApplications,AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Main where
--seems like only Main module is able to link
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width,height) (offset, offset)

background:: Color
background = black


-- pictures is a function take a list of picture and show them together
{-
drawing :: Picture
drawing = pictures
          [ translate (-20) (-100) $ color ballColor $ circleSolid 30
           , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
          ]
          where
           ballColor = dark red
           paddleColor = light( light blue)
-}
--real pong game picture
{-
drawing :: Picture
drawing = pictures [ball, walls, mkPaddle rose 120 (-20),mkPaddle orange (-120) 40]
     where
     -- The pong ball
       ball = translate (-10) 40 $ color ballColor $ circleSolid 10
       ballColor = dark red

     -- the bottom and top walls
       wall :: Float -> Picture
       wall offset = translate 0 offset $ color wallColor $ rectangleSolid 300 10

       wallColor = greyN 0.5
       walls = pictures [wall 150, wall (-150)]

     -- Make a paddle of a given border and vertical offset
       mkPaddle :: Color -> Float -> Float -> Picture
       mkPaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid 26 86
            , translate x y $ color paddleColor $ rectangleSolid 20 80
            ]

       paddleColor = light (light blue)
-}
-- now we divide the game into three parts, its state, its initial state and how we render the picture of such state
-- Data describing the state of the pong game
data PongGame = Game
     { ballLoc :: (Float, Float) --Pong ball (x,y) location.
     , ballVel :: (Float, Float) -- Pong ball (x,y) velocity
     , player1 :: Float   --Left player paddle height
     , player2 :: Float -- Right player paddle height
     }deriving Show

--PongGame is a data type, Game is the constructor of that type

-- | The starting state for the game of Pong
initialState :: PongGame
initialState = Game
               { ballLoc = ( -10, 30 )
               , ballVel = ( 10, -30 )
               , player1 = 40
               , player2 = -80
               }

render :: PongGame -- the game state to render
          -> Picture   -- A picture of this game state
render game = pictures [ball, walls,
                        mkPaddle rose 120 $ player1 game,
                        mkPaddle orange (-120) $ player2 game]
     where
       -- The pong ball.
       ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
       ballColor = dark red

       -- The bottom and top walls
       wall :: Float -> Picture
       wall offset = translate 0 offset $ color wallColor $ rectangleSolid 300 10
       wallColor = greyN 0.5
       walls = pictures [wall 150, wall (-150)]

       -- Make a paddle of a given border and vertical offset
       mkPaddle :: Color -> Float -> Float -> Picture
       mkPaddle col x y = pictures
                          [ translate x y $ color col $ rectangleSolid 26 86
                          , translate x y $ color paddleColor $ rectangleSolid 20 80]
       paddleColor = light ( light blue )


--animation part
--to create an  animation, I have to write a function which can generate a picture when given the number of seconds that have passed since the start of the animation
-- | Update the ball position using its current velocity
moveBall :: Float -- The number of seconds since last update
         -> PongGame -- the initial game state
         -> PongGame -- A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y')}
  where
    --Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New Locations
    x' = x + vx * seconds
    y' = y + vy * seconds
--helper definition
type Radius = Float
type Position = (Float, Float)

-- Detect a collision with a paddle. change the x velocity after that
paddleBounce :: PongGame -> PongGame
paddleBounce = undefined
--Detect a collision with one of the side walls. change the y velocity after that
wallBounce :: PongGame -> PongGame
wallBounce game = game {ballVel = (vx, vy')}
     where
       radius = 10
       (vx, vy) = ballVel game

       vy' | wallCollision (ballLoc game) radius = -vy
           | otherwise = vy


-- helper for wallBounce
wallCollision :: Position -> Radius -> Bool
wallCollision (_ ,y) radius = topCollision || bottomCollision
   where
     topCollision = y - radius <= (-fromIntegral height/2)
     bottomCollision = y + radius >= fromIntegral height/2

--defined the fps rate
fps :: Int
fps = 60

main :: IO ()
--static display
-- main = display window background $ render initialState
-- active display
-- main = animate window background frame
-- using simulate to do further feature
main = simulate window background fps initialState render update

update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . moveBall seconds
{-
    where
       frame :: Float -> Picture
       frame seconds = render $ moveBall seconds initialState
-}
