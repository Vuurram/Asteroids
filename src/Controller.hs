
module Controller
  ( handleInput
  , updateGame
  ) where

import Graphics.Gloss.Interface.IO.Game
import Model
import Ship
import Collision
import Movement

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

handleInput _ state = pure state


updateGame :: Float -> GameState -> IO GameState
updateGame dt state
  | paused state = pure state
  | otherwise =
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

          collisionAsteroid =
            shipCollidesAnyAsteroid updatedShip updatedAsteroids

          collisionUfo =
            shipCollidesAnyUfo updatedShip updatedUfos

      in do
        if collisionAsteroid || collisionUfo
          then putStrLn "Collision!"
          else pure ()

        pure state
          { player = updatedShip
          , asteroids = updatedAsteroids
          , ufo = updatedUfos
          , elapsedTime = elapsedTime state + dt
          }
