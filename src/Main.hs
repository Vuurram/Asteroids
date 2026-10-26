
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import View
import Controller

main :: IO ()
main =
  playIO
    (InWindow "Asteroids" (800, 600) (100, 100))
    black
    60
    initialState
    (pure . drawGame)
    handleInput
    updateGame
