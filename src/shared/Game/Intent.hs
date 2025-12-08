{-|
This module describes player actions like using items, moving or quitting the game.
|-}

module Game.Intent(Intent(..)) where

import Apecs
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)
import Game.Direction(Direction)

data Intent =
    Quit
  | Wait
  | Move Direction
  deriving (Eq, Show)
  deriving Generic

instance Serialise Intent
