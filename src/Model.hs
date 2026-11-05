module Model where

import System.Random (StdGen, randomR)

screenWidth :: Float
screenWidth = 800

screenHeight :: Float
screenHeight = 600

type Score = Int
type Lives = Int

data Position = Position
  { x :: Float
  , y :: Float
  }
  deriving (Eq, Show)

type Rotation = Float

data Ship = Ship
  { shipPosition :: Position
  , shipRotation :: Rotation
  , shipVelocity :: Position
  , shipSize     :: Float
  }
  deriving (Eq, Show)

nosePosition :: Ship -> Position
nosePosition ship =
  let Position px py = shipPosition ship
      angle = shipRotation ship * pi / 180
      distance = shipSize ship
  in Position
       (px + distance * sin angle)
       (py + distance * cos angle)

data Bullet = Bullet
  { bulletPosition :: Position
  , bulletVelocity :: Position
  , bulletSize     :: Float
  , bulletTime     :: Float
  }
  deriving (Eq, Show)

data AsteroidSize
  = Large
  | Medium
  | Small
  deriving (Eq, Show)

-- Maps asteroid sizes to their radius
asteroidRadius :: AsteroidSize -> Float
asteroidRadius Large  = 45
asteroidRadius Medium = 25
asteroidRadius Small  = 12

-- Maps asteroid sizes to their score value
asteroidPoints :: AsteroidSize -> Score
asteroidPoints Large  = 20
asteroidPoints Medium = 50
asteroidPoints Small  = 100

data Asteroid = Asteroid
  { asteroidPosition :: Position
  , asteroidVelocity :: Position
  , asteroidSize     :: AsteroidSize
  }
  deriving (Eq, Show)

data Ufo = Ufo
  { ufoPosition :: Position
  , ufoVelocity :: Position
  , ufoSize     :: Float
  }
  deriving (Eq, Show)

data Controls = Controls
  { turnLeft  :: Bool
  , turnRight :: Bool
  , thrusting :: Bool
  }
  deriving (Eq, Show)

data GameState = GameState
  { player      :: Ship
  , bullets     :: [Bullet]
  , asteroids   :: [Asteroid]
  , ufo         :: [Ufo]
  , score       :: Score
  , highScore   :: Score
  , lives       :: Lives
  , elapsedTime :: Float
  , paused      :: Bool
  , controls    :: Controls
  }
  deriving (Eq, Show)

minSpawnDistance :: Float
minSpawnDistance = 150

initialShip :: Ship
initialShip = Ship
  { shipPosition = Position 0 0
  , shipRotation = 0
  , shipVelocity = Position 0 0
  , shipSize     = 18
  }

distance :: Position -> Position -> Float
distance p1 p2 =
  sqrt ((x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2)

-- Checks if a position is valid for spawning, so not too close to the ship
validSpawnPosition :: Position -> Bool
validSpawnPosition pos =
  distance pos (Position 0 0) >= minSpawnDistance

-- Gives a random position, but not too close to the ship (this case center of screen)
randomSpawnPosition :: StdGen -> (Position, StdGen)
randomSpawnPosition gen =
  let (px, gen1) = randomR (-350, 350) gen
      (py, gen2) = randomR (-250, 250) gen1
      pos = Position px py
  in if validSpawnPosition pos
       then (pos, gen2)
       else randomSpawnPosition gen2

-- Spawn a random asteroid at a valid position, so not to close to ship
randomAsteroid :: StdGen -> (Asteroid, StdGen)
randomAsteroid gen =
  let (pos, gen1) = randomSpawnPosition gen
      (angle, gen2) = randomR (0, 2 * pi) gen1
      (speed, gen3) = randomR (30, 70) gen2
      (sizeIndex, gen4) = randomR (0 :: Int, 2) gen3

      vx = cos angle * speed
      vy = sin angle * speed

      size = case sizeIndex of
        0 -> Large
        1 -> Medium
        _ -> Small

  in (Asteroid pos (Position vx vy) size, gen4)

-- Generates a specified number of random asteroids
randomAsteroids :: Int -> StdGen -> ([Asteroid], StdGen)
randomAsteroids n gen
  | n <= 0 = ([], gen)
  | otherwise =
      let (asteroid, gen1) = randomAsteroid gen
          (rest, gen2) = randomAsteroids (n - 1) gen1
      in (asteroid : rest, gen2)

-- Generates a random UFO at a valid spawn position
randomUfo :: StdGen -> (Ufo, StdGen)
randomUfo gen =
  let (pos, gen1) = randomSpawnPosition gen
      (vx, gen2) = randomR (-20, 20) gen1
      (vy, gen3) = randomR (-20, 20) gen2
  in (Ufo pos (Position vx vy) 20, gen3)

-- Places a number n of ufos randomly on the screen
randomUfos :: Int -> StdGen -> ([Ufo], StdGen)
randomUfos n gen
  | n <= 0 = ([], gen)
  | otherwise =
      let (newUfo, gen1) = randomUfo gen
          (rest, gen2) = randomUfos (n - 1) gen1
      in (newUfo : rest, gen2)

-- Initialize the game state with random asteroids and UFOs, given a random generator and a saved high score
initialStateRandom :: StdGen -> Score -> GameState
initialStateRandom gen savedHighScore =
  let (newAsteroids, gen1) = randomAsteroids 4 gen
      (newUfos, _) = randomUfos 1 gen1
  in GameState
       { player      = initialShip
       , bullets     = []
       , asteroids   = newAsteroids
       , ufo         = newUfos
       , score       = 0
       , highScore   = savedHighScore
       , lives       = 3
       , elapsedTime = 0
       , paused      = False
       , controls    = Controls False False False
       }
