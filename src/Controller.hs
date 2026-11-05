
module Controller
  ( handleInput
  , updateGame
  ) where

import Graphics.Gloss.Interface.IO.Game
import Model
import Ship
import Bullet
import Collision
import Movement
import HighScore

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (Char 'a') Down _ _) state =
  pure state
    { controls = (controls state)
          { turnLeft = True}
        --{ turnDirection = -1 }
    }

handleInput (EventKey (Char 'd') Down _ _) state =
  pure state
    { controls = (controls state)
          {turnRight = True}
        --{ turnDirection = 1 }
    }

handleInput (EventKey (Char 'w') Down _ _) state =
  pure state
    { controls = (controls state)
        { thrusting = True }
    }

handleInput (EventKey (Char 'a') Up _ _) state =
  pure state
    { controls = (controls state)
          {turnLeft = False}
        --{ turnDirection = 0 }
    }

handleInput (EventKey (Char 'd') Up _ _) state =
  pure state
    { controls = (controls state)
        { turnRight = False}
        --{ turnDirection = 0 }
    }

handleInput (EventKey (Char 'w') Up _ _) state =
  pure state
    { controls = (controls state)
        { thrusting = False }
    }

handleInput (EventKey (Char 'p') Down _ _) state =
  pure state
    { paused = not (paused state) }

-- Shoot bullet when space is pressed, but only if the game is not paused
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state
  | paused state = pure state
  | otherwise =
      let newBullet = shoot (player state)
      in pure state
           { bullets = newBullet : bullets state
           }

handleInput _ state = pure state

-- For now still impure because of the high score update (if we make highscore abstract) and printing to the console. 
updateGame :: Float -> GameState -> IO GameState
updateGame dt state
  | paused state = pure state
  | otherwise = do
      let currentControls = controls state
          ship = player state

          turnedShip
            | turnLeft currentControls && not (turnRight currentControls) =
                rotateShip (-180 * dt) ship

            | not (turnLeft currentControls) && turnRight currentControls =
                rotateShip (180 * dt) ship

            | otherwise = ship

          acceleratedShip =
            if thrusting currentControls
              then accelerateShip dt turnedShip
              else turnedShip

          updatedShip = updateShip dt acceleratedShip

          updatedAsteroids = updateAsteroids dt (asteroids state)
          updatedUfos = updateUfos dt (ufo state)

          movedBullets = updateBullets dt (bullets state)

          (remainingBullets, remainingAsteroids, earnedPoints) =
            bulletAsteroidCollisions movedBullets updatedAsteroids

          newScore = score state + earnedPoints

          collisionAsteroid =
            shipCollidesAnyAsteroid updatedShip remainingAsteroids

          collisionUfo =
            shipCollidesAnyUfo updatedShip updatedUfos

      newHighScore <- updateHighScore "highscore.txt" newScore

      if collisionAsteroid || collisionUfo
        then putStrLn "Collision!"
        else pure ()

      pure state
        { player = updatedShip
        , asteroids = remainingAsteroids
        , ufo = updatedUfos
        , bullets = remainingBullets
        , score = newScore
        , highScore = newHighScore
        , elapsedTime = elapsedTime state + dt
        }
