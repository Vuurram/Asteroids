
module View
  ( drawGame
  ) where

import Graphics.Gloss
import Model

-- Draws every component of game
drawGame state =
  pictures
    (map drawAsteroid (asteroids state)
      ++ [drawShip (player state) | gameStatus state == Playing && shouldDrawShip state] --if it's dead or dying, shouldn't draw the ship
      ++ map drawUfo (ufo state)
      ++ [drawScore (score state)]
      ++ [drawHighScore (highScore state)]
      ++ [drawTime (elapsedTime state)]
      ++ [drawPause (paused state)]
      ++ map drawBullet (bullets state)
      ++ [drawDeathAnimation state | deathTimer state > 0] --stop drawing when the death timer is 0, so the animation stops.
      ++ [drawGameOver state | gameStatus state == GameOver] --only draw if game over
      ++ [drawLives (lives state)]  
    )

-- Logic for drawing the ship
drawShip :: Ship -> Picture
drawShip ship =
  let Position px py = shipPosition ship
  in pictures
       [ translate px py
           (rotate (shipRotation ship)
             (color white shipShape))
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
       ]
     

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

-- Logic for drawing the game over message
drawGameOver :: GameState -> Picture
drawGameOver _ =
  pictures
    [ translate (-150) 50 $
        scale 0.5 0.5 $
          color red $
            text "GAME OVER"

    , translate (-130) (-30) $
        scale 0.2 0.2 $
          color white $
            text "Press R to restart"
    ]

-- Draws the remaining lives
drawLives :: Lives -> Picture
drawLives remainingLives =
  pictures
    [ translate (300 + fromIntegral i * 30) 250 $
        scale 0.7 0.7 $
          color white shipShape
    | i <- [0 .. remainingLives - 1]
    ]

drawDeathAnimation :: GameState -> Picture
drawDeathAnimation state =
  let Position px py = shipPosition (player state)

      progress = 1 - deathTimer state -- how much bigger the circle should become over time
      outerRadius = 8 + progress * 45
      innerRadius = 4 + progress * 25

  in translate px py $
       pictures
         [ color orange (circle outerRadius)
         , color yellow (circle innerRadius)
         ]

--this is to create a blinking effect for the ship when it is invincible after respawning. The ship will blink on and off at a certain speed, which is determined by the invincibility timer. 
shouldDrawShip :: GameState -> Bool
shouldDrawShip state
  | invincibilityTimer state <= 0 = True
  | otherwise =
      let blinkSpeed = 8 -- Adjust this value to change the blink speed. The higher the value, the faster the blinking.
          blinkPhase = floor (invincibilityTimer state * blinkSpeed) :: Int --this works by multiplying by the invincibility timer, which will at consistent intervals switch between an even and uneven number. We can then draw, or not draw, the ship based on the number we get, at a consistent rate
      in even blinkPhase