module Bullet
  ( shoot
    , bulletAsteroidCollisions
  ) where

import Model
import Asteroids
import Collision

dbulletSpeed :: Float
dbulletSpeed = 400

dbulletSize :: Float
dbulletSize = 3

dbulletTime :: Float
dbulletTime = 0.75

-- Shoots a bullet from the ship's nose position in the direction the ship is facing
shoot :: Ship -> Bullet
shoot ship =
  let position = nosePosition ship
      angle = shipRotation ship * pi / 180

      velocity = Position
        (dbulletSpeed * sin angle)
        (dbulletSpeed * cos angle)

  in Bullet position velocity dbulletSize dbulletTime

-- Keeps track of collisions between bullets and asteroids
-- Returns the remaining bullets, remaining asteroids, and the score earned from destroyed asteroids
bulletAsteroidCollisions :: [Bullet] -> [Asteroid] -> ([Bullet], [Asteroid], Score)
bulletAsteroidCollisions bullets asteroids =
  (remainingBullets, remainingAsteroids ++ newAsteroids, earnedPoints)
  where
    hits = 
      [ (bullet, asteroid)
      | bullet <- bullets
      , asteroid <- asteroids
      , bulletAsteroidCollision bullet asteroid
      ]

    hitBullets = map fst hits
    hitAsteroids = map snd hits

    remainingBullets =
      filter (`notElem` hitBullets) bullets

    remainingAsteroids =
      filter (`notElem` hitAsteroids) asteroids

    newAsteroids =
      concatMap splitAsteroid hitAsteroids
    
    earnedPoints =
      sum (map (asteroidPoints . asteroidSize) hitAsteroids)