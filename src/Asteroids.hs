module Asteroids where

import Model

-- Split the asteroid into two smaller asteroids when hit by a bullet
-- If asteroid is already small, it will be destroyed and not split further
splitAsteroid :: Asteroid -> [Asteroid]
splitAsteroid asteroid =
  let newSize = case asteroidSize asteroid of
        Large  -> Just Medium
        Medium -> Just Small
        Small  -> Nothing

      Position px py = asteroidPosition asteroid
      Position vx vy = asteroidVelocity asteroid

  in case newSize of
       Nothing -> []
       Just size ->
         [ Asteroid
             (Position px py)
             (Position (-vy) vx)
             size
         , Asteroid
             (Position px py)
             (Position vy (-vx))
             size
         ] 