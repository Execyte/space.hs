module Intent(Intent(..)) where

import Apecs
import Direction(Direction)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)
import qualified Direction as Dir

data Intent =
    Quit
  | Wait
  | Move Direction
  deriving (Eq, Show, Generic)

instance Serialise Intent
