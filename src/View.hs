
module View
  ( drawGame
  ) where

import Graphics.Gloss
import Model

-- Teken de volledige spelwereld
drawGame :: GameState -> Picture
drawGame state =
  pictures
    (map drawAsteroid (asteroids state)
      ++ [drawShip (player state)])

-- Teken het ruimteschip als een driehoek
drawShip :: Ship -> Picture
drawShip ship =
  let Position px py = shipPosition ship
  in pictures
       [ translate px py
           (rotate (shipRotation ship)
             (color white shipShape))
       , drawBoundingCircle (shipPosition ship) (shipSize ship)
       ]

-- De punt van het schip wijst standaard omhoog
shipShape :: Picture
shipShape =
  line
    [ (0, 18)
    , (-12, -12)
    , (0, -7)
    , (12, -12)
    , (0, 18)
    ]

-- Teken een asteroïde als een onregelmatige veelhoek
drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid =
  let Position ax ay = asteroidPosition asteroid
      size = asteroidSize asteroid
  in pictures
       [ translate ax ay
           (color (greyN 0.65)
             (scale size size asteroidShape))
       , drawBoundingCircle (asteroidPosition asteroid) size
       ]

-- Maakt de bounding boxes zichtbaar voor debugging         
drawBoundingCircle :: Position -> Float -> Picture
drawBoundingCircle position radius =
  let Position px py = position
  in translate px py
       (color red (circle radius))       

-- Vorm van een asteroïde
asteroidShape :: Picture
asteroidShape =
  line
    [ (-0.8, 0.2)
    , (-0.5, 0.8)
    , (0.1, 0.9)
    , (0.7, 0.5)
    , (0.9, -0.2)
    , (0.4, -0.8)
    , (-0.3, -0.9)
    , (-0.8, -0.4)
    , (-0.8, 0.2)
    ]
