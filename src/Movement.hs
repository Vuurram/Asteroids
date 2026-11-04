module Movement
  ( wrapPosition
  ) where

import Model
import Data.Fixed (mod')

wrapPosition :: Position -> Position
wrapPosition (Position px py) =
  Position
    { x = (px + screenWidth / 2) `mod'` screenWidth - screenWidth / 2
    , y = (py + screenHeight / 2) `mod'` screenHeight - screenHeight / 2
    }