module Intent(Intent(..)) where

import Apecs
import Direction

data Intent =
    Wait
  | Quit
  | Help Entity
  | Disarm Entity
  | Harm Entity
  | Grab Entity
  | Use Entity
  | LookAt Entity
  | Move Direction
  deriving (Eq, Show)
