
module View
  ( drawGame
  ) where

import Graphics.Gloss
import Model

-- Draws every component of game
drawGame state =
  pictures
    (map drawAsteroid (asteroids state)
      ++ [drawShip (player state)]
      ++ map drawUfo (ufo state)
      ++ [drawScore (score state)]
      ++ [drawHighScore (highScore state)]
      ++ [drawTime (elapsedTime state)]
      ++ [drawPause (paused state)]
      ++ map drawBullet (bullets state)
    )

-- Logic for drawing the ship
drawShip :: Ship -> Picture
drawShip ship =
  let Position px py = shipPosition ship
  in pictures
       [ translate px py
           (rotate (shipRotation ship)
             (color white shipShape))
       , drawBoundingCircle (shipPosition ship) (shipSize ship)
       ]

-- Logic for drawing the bullet
drawBullet :: Bullet -> Picture
drawBullet bullet =
  let Position bx by = bulletPosition bullet
  in translate bx by
       (color white (circleSolid (bulletSize bullet)))

-- Logic for drawing an asteroid
drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid =
  let position@(Position px py) = asteroidPosition asteroid
      radius = asteroidRadius (asteroidSize asteroid)
  in pictures
       [ translate px py
           (scale radius radius
             (color white asteroidShape))
       , drawBoundingCircle position radius
       ]

-- Logic for drawing an ufo
drawUfo :: Ufo -> Picture
drawUfo ufo =
  let Position ax ay = ufoPosition ufo
      size = ufoSize ufo
  in pictures
       [ translate ax ay
           (color (greyN 0.5)
             (scale size size ufoShape))
       , drawBoundingCircle (ufoPosition ufo) size
       ]

-- Logic for drawing the boundingcircles of entities (debugging purposes)        
drawBoundingCircle :: Position -> Float -> Picture
drawBoundingCircle position radius =
  let Position px py = position
  in translate px py
       (color red (circle radius))       

-- Shapes for the game entities

shipShape :: Picture
shipShape =
  line
    [ (0, 18)
    , (-12, -12)
    , (0, -7)
    , (12, -12)
    , (0, 18)
    ]

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

ufoShape :: Picture
ufoShape =
  line
    [ (-1.0, 0)
    , (-0.7, 0.3)
    , (-0.4, 0.3)
    , (-0.25, 0.6)
    , (0.25, 0.6)
    , (0.4, 0.3)
    , (0.7, 0.3)
    , (1.0, 0)
    , (0.7, -0.3)
    , (-0.7, -0.3)
    , (-1.0, 0)
    ]

-- Logic for drawing the score
drawScore :: Score -> Picture
drawScore score = translate (-390) 230 $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show score)

-- Logic for drawing the high score
drawHighScore :: Score -> Picture
drawHighScore highScore = translate (-390) 260 $ scale 0.15 0.15 $ color white $ text ("High Score: " ++ show highScore)

-- Logic for drawing the elapsed time
drawTime :: Float -> Picture
drawTime time =
  let seconds = floor time :: Int
  in translate (-390) 200 $ scale 0.15 0.15 $ color white $ text ("Time: " ++ show seconds ++ "s")

-- Logic for drawing the pause message
drawPause :: Bool -> Picture
drawPause True  = translate (-75) 0 $ scale 0.3 0.3 $ color red $ text "PAUSED"
drawPause False = blank    
