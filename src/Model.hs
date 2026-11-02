
module Model where

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
  }
  deriving (Eq, Show)

data Asteroid = Asteroid
  { asteroidPosition :: Position
  , asteroidVelocity :: Position
  , asteroidSize     :: Float
  }
  deriving (Eq, Show)

--data Controls = Controls
--  { turnDirection :: Float
--  , thrusting     :: Bool
--  }
--  deriving (Eq, Show)

data Controls = Controls
  { turnLeft      :: Bool
  , turnRight     :: Bool
  , thrusting     :: Bool
  }
  deriving (Eq, Show)

data GameState = GameState
  { player    :: Ship
  , asteroids :: [Asteroid]
  , score     :: Score
  , lives     :: Lives
  , paused    :: Bool
  , controls  :: Controls
  }
  deriving (Eq, Show)

initialShip :: Ship
initialShip = Ship
  { shipPosition = Position 0 0
  , shipRotation = 0
  , shipVelocity = Position 0 0
  }

initialAsteroids :: [Asteroid]
initialAsteroids =
  [ Asteroid (Position (-250) 150) (Position 15 (-10)) 35
  , Asteroid (Position 220 100) (Position (-20) (-12)) 25
  , Asteroid (Position 150 (-180)) (Position (-10) 18) 45
  , Asteroid (Position (-200) (-120)) (Position 12 14) 30
  ]

initialState :: GameState
initialState = GameState
  { player = initialShip
  , asteroids = initialAsteroids
  , score = 0
  , lives = 3
  , paused = False
  , controls = Controls
      { turnLeft = False
      , turnRight = False
      , thrusting = False
      }
  }
