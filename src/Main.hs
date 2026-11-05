
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import View
import Controller
import System.Random (newStdGen)
import HighScore (getHighScore)

main :: IO ()
main = do
  gen <- newStdGen
  highScore <- getHighScore "highscore.txt"
  let initialState = initialStateRandom gen highScore -- Initialize the game state with a random generator and the high score

  playIO
    (InWindow "Asteroids" (round screenWidth, round screenHeight) (100, 100))
    black
    60
    initialState
    (pure . drawGame)
    handleInput
    updateGame
