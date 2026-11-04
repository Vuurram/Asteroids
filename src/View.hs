
module View
  ( drawGame
  ) where

import Graphics.Gloss
import Model

drawGame state =
  pictures
    (map drawAsteroid (asteroids state)
      ++ [drawShip (player state)]
      ++ map drawUfo (ufo state)
      ++ [drawScore (score state)]
      ++ [drawHighScore (highScore state)]
      ++ [drawTime (elapsedTime state)]
      ++ [drawPause (paused state)]
    )

drawShip :: Ship -> Picture
drawShip ship =
  let Position px py = shipPosition ship
  in pictures
       [ translate px py
           (rotate (shipRotation ship)
             (color white shipShape))
       , drawBoundingCircle (shipPosition ship) (shipSize ship)
       ]

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
         
drawBoundingCircle :: Position -> Float -> Picture
drawBoundingCircle position radius =
  let Position px py = position
  in translate px py
       (color red (circle radius))       

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

drawScore :: Score -> Picture
drawScore score = translate (-390) 230 $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show score)

drawHighScore :: Score -> Picture
drawHighScore highScore = translate (-390) 260 $ scale 0.15 0.15 $ color white $ text ("High Score: " ++ show highScore)

drawTime :: Float -> Picture
drawTime time =
  let seconds = floor time :: Int
  in translate (-390) 200 $ scale 0.15 0.15 $ color white $ text ("Time: " ++ show seconds ++ "s")

drawPause :: Bool -> Picture
drawPause True  = translate (-75) 0 $ scale 0.3 0.3 $ color red $ text "PAUSED"
drawPause False = blank    
