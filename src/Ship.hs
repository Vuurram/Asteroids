
module Ship
  ( rotateShip
  , accelerateShip
  , updateShip
  ) where

import Model

--rotationSpeed :: Float
--rotationSpeed = 180

acceleration :: Float
acceleration = 120

rotateShip :: Float -> Ship -> Ship
rotateShip angle ship =
  ship
    { shipRotation = shipRotation ship + angle
    }

accelerateShip :: Float -> Ship -> Ship
accelerateShip dt ship =
  let angle = shipRotation ship * pi / 180
      velocity = shipVelocity ship
      newVelocity = Position
        { x = x velocity - sin angle * acceleration * dt
        , y = y velocity + cos angle * acceleration * dt
        }
  in ship { shipVelocity = newVelocity }

updateShip :: Float -> Ship -> Ship
updateShip dt ship =
  let position = shipPosition ship
      velocity = shipVelocity ship
      newPosition = Position
        { x = x position + x velocity * dt
        , y = y position + y velocity * dt
        }
  in ship { shipPosition = newPosition }
