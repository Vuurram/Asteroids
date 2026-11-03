module Collision where

import Model

distanceSquared :: Position -> Position -> Float
distanceSquared p1 p2 =
    let dx = x p1 - x p2
        dy = y p1 - y p2
    in dx * dx + dy * dy

circlesCollide :: Position -> Float -> Position -> Float -> Bool
circlesCollide p1 r1 p2 r2 =
    distanceSquared p1 p2 <= (r1 + r2) ^ 2

shipCollidesAsteroid :: Ship -> Asteroid -> Bool
shipCollidesAsteroid ship asteroid =
  circlesCollide
    (shipPosition ship) (shipSize ship)
    (asteroidPosition asteroid) (asteroidSize asteroid)

shipCollidesAnyAsteroid :: Ship -> [Asteroid] -> Bool
shipCollidesAnyAsteroid ship =
  any (shipCollidesAsteroid ship)

shipCollidesUfo :: Ship -> Ufo -> Bool
shipCollidesUfo ship ufo =
  circlesCollide
    (shipPosition ship) (shipSize ship)
    (ufoPosition ufo) (ufoSize ufo)

shipCollidesAnyUfo :: Ship -> [Ufo] -> Bool
shipCollidesAnyUfo ship =
  any (shipCollidesUfo ship)