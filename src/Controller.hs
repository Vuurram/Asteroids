
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
--handle restarting the game when 'r' is pressed
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

--when the player dies, takes some time to respawn.
updateDying :: Float -> GameState -> IO GameState
updateDying dt state = do
  let updatedAsteroids = updateAsteroids dt (asteroids state)
      updatedUfos = updateUfos dt (player state) (ufo state)
      updatedBullets = updateBullets dt (bullets state)

      remainingTime = deathTimer state - dt

  if remainingTime <= 0
    then
      pure state
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
    else --the world should still update while the player is dead, so we update the asteroids, UFOs, and bullets, but not the player.
      pure state
        { asteroids = updatedAsteroids
        , ufo = updatedUfos
        , bullets = updatedBullets
        , deathTimer = remainingTime
        , elapsedTime = elapsedTime state + dt
        }

updatePlaying :: Float -> GameState -> IO GameState 
updatePlaying dt state = do
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

      -- Check whether the player is allowed to collide with enemies.
      canCollide = invincibilityTimer state <= 0

      collisionAsteroid =
        canCollide &&
        shipCollidesAnyAsteroid updatedShip remainingAsteroids

      collisionUfo =
        canCollide &&
        shipCollidesAnyUfo updatedShip remainingUfos

      playerCollided = collisionAsteroid || collisionUfo

      -- Remove one life if the player collided with something.
      newLives =
        if playerCollided then lives state - 1 else lives state

      -- Determine what happens after the collision.
      newStatus
        | playerCollided && newLives <= 0 = GameOver
        | playerCollided = Dying
        | otherwise = Playing

      newInvincibilityTimer =
        max 0 (invincibilityTimer state - dt)

  newHighScore <- updateHighScore "highscore.txt" newScore

  -- Start a new wave if all asteroids and UFOs are destroyed.
  (newAsteroids, newUfos, newPlayer) <-
    if null remainingAsteroids && null remainingUfos
      then do
        gen <- newStdGen
        let (spawnedAsteroids, gen1) = randomAsteroids 4 gen
            (spawnedUfos, _) = randomUfos 1 gen1
        pure (spawnedAsteroids, spawnedUfos, initialShip)
      else
        pure (remainingAsteroids, remainingUfos, updatedShip)

  pure state
    { player = newPlayer
    , asteroids = newAsteroids
    , ufo = newUfos
    , bullets = finalBullets
    , score = newScore
    , highScore = newHighScore
    , lives = newLives
    , gameStatus = newStatus
    , deathTimer =
        if playerCollided then deathDuration else 0 --we want animations to still happen when it's game over.
    , invincibilityTimer =
        if playerCollided then 0 else newInvincibilityTimer
    , elapsedTime = elapsedTime state + dt
    }

-- Keep the world moving after Game Over, instead of freezing everything.
updateGameOver :: Float -> GameState -> IO GameState
updateGameOver dt state =
  pure state
    { asteroids = updateAsteroids dt (asteroids state)
    , ufo = updateUfos dt (player state) (ufo state)
    , bullets = updateBullets dt (bullets state)
    , deathTimer = max 0 (deathTimer state - dt)
    , elapsedTime = elapsedTime state + dt
    }


-- For now still impure because of the high score update (if we make highscore abstract) and printing to the console. 
updateGame :: Float -> GameState -> IO GameState
updateGame dt state
  | gameStatus state == GameOver = updateGameOver dt state
  | paused state = pure state
  | gameStatus state == Dying = updateDying dt state
  | otherwise = updatePlaying dt state
 
