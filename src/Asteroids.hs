module Asteroids where

import Model

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