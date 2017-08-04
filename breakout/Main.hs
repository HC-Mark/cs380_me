{-
This program build up a simple "breakout" game. Player will launch a small ball toward bricks, if ball touch the brick, brick will disappear, and ball bounces back. If the ball pass the paddle and touch the bottom of window, then the game will stop.

citation: Richard Eisenberg: Pong.hs (sample code provided in class)
	  TetrisHaskellWeekend: Logic.hs Main.hs Piece.hs Playfield.hs Renderer.hs State.hs (I use some logic from tetris to help me build up brickList, separate my initial program into several parts and build up score system) (https://github.com/mgeorgoulopoulos/TetrisHaskellWeekend)
-}
module Main where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid
import Renderer
import State
import Event
import Brick

window :: Display
window  = InWindow "Breakout" (fieldWidth,fieldHeight) (200,200)

background :: Color
background = white

fps = 60

main :: IO ()
main = play window
            background
            fps
            initialWorld
            render
            react
            event