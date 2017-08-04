module State where

import Brick

data World = World { paddle_loc :: Float
                   , ball_loc    :: (Float, Float)
                   , ball_motion :: (Float, Float)
                   , paddle_motion  :: Float
                   , playing :: Bool
                   , w_brickList :: [Brick]
		   , totalScore :: Int}

initialWorld = World { paddle_loc = 0
                     , ball_loc    = (0,-fieldHeightF/2)
                     , ball_motion = (ballSpeed, 0)
                     , paddle_motion  = 0
                     , playing = False
                     , w_brickList = brickList
		     , totalScore = 0}