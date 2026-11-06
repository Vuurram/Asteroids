module Collision where

import Model

distanceSquared :: Position -> Position -> Float
distanceSquared p1 p2 =
    let dx = x p1 - x p2
        dy = y p1 - y p2
    in dx * dx + dy * dy

-- Check if two circles collide based on their positions and radius
circlesCollide :: Position -> Float -> Position -> Float -> Bool
circlesCollide p1 r1 p2 r2 =
    distanceSquared p1 p2 <= (r1 + r2) ^ 2

shipCollidesAnyAsteroid :: Ship -> [Asteroid] -> Bool
shipCollidesAnyAsteroid ship =
    any (shipAsteroidCollision ship)

shipAsteroidCollision :: Ship -> Asteroid -> Bool
shipAsteroidCollision ship asteroid =
    circlesCollide
        (shipPosition ship) (shipSize ship)
        (asteroidPosition asteroid) (asteroidRadius (asteroidSize asteroid))

shipCollidesAnyUfo :: Ship -> [Ufo] -> Bool
shipCollidesAnyUfo ship =
    any (shipUfoCollision ship)

shipUfoCollision :: Ship -> Ufo -> Bool
shipUfoCollision ship ufo =
    circlesCollide
        (shipPosition ship) (shipSize ship)
        (ufoPosition ufo) (ufoSize ufo)

bulletAsteroidCollision :: Bullet -> Asteroid -> Bool
bulletAsteroidCollision bullet asteroid =
    circlesCollide
        (bulletPosition bullet) (bulletSize bullet)
        (asteroidPosition asteroid) (asteroidRadius (asteroidSize asteroid))

--for bullet with ufo collision
bulletUfoCollision :: Bullet -> Ufo -> Bool
bulletUfoCollision bullet ufo =
    circlesCollide
        (bulletPosition bullet) (bulletSize bullet)
        (ufoPosition ufo) (ufoSize ufo)
