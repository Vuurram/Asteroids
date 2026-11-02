module Collision where

import Model

distanceSquared :: Position -> Position -> Float
distanceSquared p1 p2 =
    let dx = x p1 - x p2
        dy = y p1 - y p2
    in dx * dx + dy * dy

circlesCollide :: Position -> Float -> Position -> Float -> Bool
circlesCollide p1 r1 p2 r2 =
    distanceSquared p1 p2 <= (r1 + r2) ^ 2