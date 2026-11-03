
module Controller
  ( handleInput
  , updateGame
  ) where

import Graphics.Gloss.Interface.IO.Game
import Model
import Ship
import Collision

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

handleInput _ state = pure state

updateGame :: Float -> GameState -> IO GameState
updateGame dt state =
  let currentControls = controls state
      ship = player state

      -- Check for multiple rotation inputs
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

      collisionAsteroid =
        shipCollidesAnyAsteroid updatedShip (asteroids state)
      
      collisionUfo =
        shipCollidesAnyUfo updatedShip (ufo state)

  in do
    if collisionAsteroid || collisionUfo -- For debugging purposes
      then putStrLn "Collision!"
      else pure ()

    pure state { player = updatedShip }
