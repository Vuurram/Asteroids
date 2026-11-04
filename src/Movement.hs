module Movement where

import Data.Fixed (mod')
import Model

wrapPosition :: Position -> Position
wrapPosition (Position px py) =
  Position
    { x = (px + screenWidth / 2) `mod'` screenWidth - screenWidth / 2
    , y = (py + screenHeight / 2) `mod'` screenHeight - screenHeight / 2
    }

updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid dt asteroid =
  let oldPosition = asteroidPosition asteroid
      velocity = asteroidVelocity asteroid
      newPosition = Position
        { x = x oldPosition + x velocity * dt
        , y = y oldPosition + y velocity * dt
        }
  in asteroid
       { asteroidPosition = wrapPosition newPosition }

updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids dt = map (updateAsteroid dt)

updateUfo :: Float -> Ufo -> Ufo
updateUfo dt enemy =
  let oldPosition = ufoPosition enemy
      velocity = ufoVelocity enemy
      newPosition = Position
        { x = x oldPosition + x velocity * dt
        , y = y oldPosition + y velocity * dt
        }
  in enemy
       { ufoPosition = wrapPosition newPosition }

updateUfos :: Float -> [Ufo] -> [Ufo]
updateUfos dt = map (updateUfo dt)
