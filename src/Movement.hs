module Movement where

import Data.Fixed (mod')
import Model

-- Keeps the position of an object within the screen boundaries by wrapping around
wrapPosition :: Position -> Position
wrapPosition (Position px py) =
  Position
    { x = (px + screenWidth / 2) `mod'` screenWidth - screenWidth / 2
    , y = (py + screenHeight / 2) `mod'` screenHeight - screenHeight / 2
    }

-- Updates the position of a asteroid
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

-- Updates the position for all asteroids
updateAsteroids :: Float -> [Asteroid] -> [Asteroid]
updateAsteroids dt = map (updateAsteroid dt)

-- Updates the position of a ufo
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

-- Updates the position for all ufos
updateUfos :: Float -> [Ufo] -> [Ufo]
updateUfos dt = map (updateUfo dt)

-- Updates the position of a bullet as long as the Lifetime is nof over
updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets dt bullets =
  map moveBullet (filter isAlive bullets)
  where
    moveBullet bullet =
      let Position px py = bulletPosition bullet
          Position vx vy = bulletVelocity bullet

      in bullet
           { bulletPosition = Position
               (px + vx * dt)
               (py + vy * dt)
           , bulletTime = bulletTime bullet - dt
           }

    isAlive bullet =
      bulletTime bullet > dt

