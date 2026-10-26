
module Controller
  ( handleInput
  , updateGame
  ) where

import Graphics.Gloss.Interface.IO.Game
import Model
import Ship

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (Char 'a') Down _ _) state =
  pure state
    { controls = (controls state)
        { turnDirection = 1 }
    }

handleInput (EventKey (Char 'd') Down _ _) state =
  pure state
    { controls = (controls state)
        { turnDirection = -1 }
    }

handleInput (EventKey (Char 'w') Down _ _) state =
  pure state
    { controls = (controls state)
        { thrusting = True }
    }

handleInput (EventKey (Char 'a') Up _ _) state =
  pure state
    { controls = (controls state)
        { turnDirection = 0 }
    }

handleInput (EventKey (Char 'd') Up _ _) state =
  pure state
    { controls = (controls state)
        { turnDirection = 0 }
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

      turnedShip =
        rotateShip
          (turnDirection currentControls * 180 * dt)
          ship

      acceleratedShip =
        if thrusting currentControls
          then accelerateShip dt turnedShip
          else turnedShip

      updatedShip = updateShip dt acceleratedShip

  in pure state { player = updatedShip }
