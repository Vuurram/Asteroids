module Ship
  ( rotateShip
  , accelerateShip
  , updateShip
  ) where

import Model
import Movement

--rotationSpeed :: Float
--rotationSpeed = 180

acceleration :: Float
acceleration = 120

-- Rotates the ship with angle
rotateShip :: Float -> Ship -> Ship
rotateShip angle ship =
  ship
    { shipRotation = shipRotation ship + angle
    }

-- Accelerates the ship in the direction it is facing
accelerateShip :: Float -> Ship -> Ship
accelerateShip dt ship =
  let angle       = shipRotation ship * pi / 180
      velocity    = shipVelocity ship
      newVelocity = Position
        { x = x velocity + sin angle * acceleration * dt
        , y = y velocity + cos angle * acceleration * dt
        }
  in ship { shipVelocity = newVelocity }

friction :: Float
friction = 0.5 -- Friction coefficient

-- updates the position of the ship based on its velocity 
-- Uses a friction to makes sure it doesn't move forever
updateShip :: Float -> Ship -> Ship
updateShip dt ship =
  let position = shipPosition ship
      velocity = shipVelocity ship

      newVelocity = Position
        { x = x velocity * (1 - friction * dt)
        , y = y velocity * (1 - friction * dt)
        }

      newPosition = Position
        { x = x position + x velocity * dt
        , y = y position + y velocity * dt
        }

  in ship { shipPosition = wrapPosition newPosition, shipVelocity = newVelocity }
