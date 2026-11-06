
module Controller
  ( handleInput
  , updateGame
  ) where

import Graphics.Gloss.Interface.IO.Game
import Model
import Ship
import Bullet
import Collision
import Movement
import HighScore
import System.Random (newStdGen)

deathDuration :: Float
deathDuration = 1.0 -- Duration of the death animation in seconds

invincibilityDuration :: Float
invincibilityDuration = 3.0 -- Duration of invincibility after respawn in seconds

handleInput :: Event -> GameState -> IO GameState
-- Handle restarting the game when 'r' is pressed
handleInput (EventKey (Char 'r') Down _ _) state 
  | gameStatus state == GameOver = do
      gen <- newStdGen
      pure (initialStateRandom gen (highScore state)) -- Reset the game state with a new random generator and the current high score

handleInput (EventKey (Char 'a') Down _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise = 
    pure state
      { controls = (controls state)
            { turnLeft = True}
      }

handleInput (EventKey (Char 'd') Down _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise = 
    pure state
      { controls = (controls state)
            {turnRight = True}
      }

handleInput (EventKey (Char 'w') Down _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise = 
      pure state
      { controls = (controls state)
          { thrusting = True }
      }

handleInput (EventKey (Char 'a') Up _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise = 
    pure state
     { controls = (controls state)
            {turnLeft = False}
      }

handleInput (EventKey (Char 'd') Up _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise =
     pure state
      { controls = (controls state)
          { turnRight = False}
      }

handleInput (EventKey (Char 'w') Up _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise =
      pure state
        { controls = (controls state)
        { thrusting = False }
        }

handleInput (EventKey (Char 'p') Down _ _) state 
  | gameStatus state /= Playing = pure state
  | otherwise =
      pure state
       { paused = not (paused state) }

-- Shoot bullet when space is pressed, but only if the game is not paused
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state
  | gameStatus state /= Playing = pure state
  | paused state = pure state
  | otherwise =
      let newBullet = shoot (player state)
      in pure state
           { bullets = newBullet : bullets state
           }

handleInput _ state = pure state

-- When the player dies, takes some time to respawn.
--when the player dies, takes some time to respawn.
updateDying :: Float -> GameState -> GameState
updateDying dt state =
  let updatedAsteroids = updateAsteroids dt (asteroids state)
      updatedUfos = updateUfos dt (player state) (ufo state)
      updatedBullets = updateBullets dt (bullets state)
      remainingTime = deathTimer state - dt

  in if remainingTime <= 0
       then
         state
           { player = initialShip
           , asteroids = updatedAsteroids
           , ufo = updatedUfos
           , bullets = updatedBullets
           , gameStatus = Playing
           , deathTimer = 0
           , invincibilityTimer = invincibilityDuration
           , controls = Controls False False False
           , elapsedTime = elapsedTime state + dt
           }
       else
         state
           { asteroids = updatedAsteroids
           , ufo = updatedUfos
           , bullets = updatedBullets
           , deathTimer = remainingTime
           , elapsedTime = elapsedTime state + dt
           }

-- Updates the game state based on the current game status
updatePlaying :: Float -> GameState -> GameState
updatePlaying dt state =
  let currentControls = controls state
      ship = player state

      turnedShip
        | turnLeft currentControls && not (turnRight currentControls) =
            rotateShip (-180 * dt) ship
        | not (turnLeft currentControls) && turnRight currentControls =
            rotateShip (180 * dt) ship
        | otherwise = ship

      acceleratedShip =
        if thrusting currentControls
          then accelerateShip dt turnedShip
          else turnedShip

      updatedShip = updateShip dt acceleratedShip

      updatedAsteroids = updateAsteroids dt (asteroids state)
      updatedUfos = updateUfos dt updatedShip (ufo state)
      movedBullets = updateBullets dt (bullets state)

      (remainingBullets, remainingAsteroids, earnedPoints) =
        bulletAsteroidCollisions movedBullets updatedAsteroids

      (finalBullets, remainingUfos, ufoPoints) =
        bulletUfoCollisions remainingBullets updatedUfos

      newScore = score state + earnedPoints + ufoPoints
      newHighScore = max (highScore state) newScore

      canCollide = invincibilityTimer state <= 0

      collisionAsteroid =
        canCollide &&
        shipCollidesAnyAsteroid updatedShip remainingAsteroids

      collisionUfo =
        canCollide &&
        shipCollidesAnyUfo updatedShip remainingUfos

      playerCollided = collisionAsteroid || collisionUfo

      newLives =
        if playerCollided then lives state - 1 else lives state

      newStatus
        | playerCollided && newLives <= 0 = GameOver
        | playerCollided = Dying
        | otherwise = Playing

      newInvincibilityTimer =
        max 0 (invincibilityTimer state - dt)

  in state
       { player = updatedShip
       , asteroids = remainingAsteroids
       , ufo = remainingUfos
       , bullets = finalBullets
       , score = newScore
       , highScore = newHighScore
       , lives = newLives
       , gameStatus = newStatus
       , deathTimer =
           if playerCollided then deathDuration else 0
       , invincibilityTimer =
           if playerCollided then 0 else newInvincibilityTimer
       , elapsedTime = elapsedTime state + dt
       }

-- Keep the world moving after Game Over, instead of freezing everything.
updateGameOver :: Float -> GameState -> GameState
updateGameOver dt state =
  state
    { asteroids = updateAsteroids dt (asteroids state)
    , ufo = updateUfos dt (player state) (ufo state)
    , bullets = updateBullets dt (bullets state)
    , deathTimer = max 0 (deathTimer state - dt)
    , elapsedTime = elapsedTime state + dt
    }

-- Impure wrapper for updating the game. Keeping the rest pure.
updateGame :: Float -> GameState -> IO GameState
updateGame dt state
  | paused state = pure state
  | gameStatus state == GameOver =
      pure (updateGameOver dt state)
  | gameStatus state == Dying =
      pure (updateDying dt state)
  | otherwise = do
      let updatedState = updatePlaying dt state

      if gameStatus updatedState == GameOver
        then do
          savedHighScore <- updateHighScore "highscore.txt" (score updatedState)
          pure updatedState
            { highScore = savedHighScore }
        else if null (asteroids updatedState) && null (ufo updatedState)
          then do
            gen <- newStdGen
            let (spawnedAsteroids, gen1) = randomAsteroids 4 gen
                (spawnedUfos, _) = randomUfos 1 gen1
            pure updatedState
              { asteroids = spawnedAsteroids
              , ufo = spawnedUfos
              , player = initialShip
              }
          else
            pure updatedState
 
