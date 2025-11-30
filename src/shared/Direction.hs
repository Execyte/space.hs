module Direction(Direction(..)) where

import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

data Direction = Up | Down | Left | Right deriving (Eq, Show, Enum, Generic)

instance Serialise Direction
