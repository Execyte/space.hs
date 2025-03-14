module Shared.Components.Position (Position) where

import Linear (V2)
import Apecs

newtype Position = Position (V2 Double)

instance Component Position where type Storage Position = Map Position
