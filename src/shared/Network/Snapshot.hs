module Network.Snapshot(ComponentSnapshot(..)) where

import Game.Components
import GHC.Generics(Generic)
import Codec.Serialise(Serialise)

data ComponentSnapshot = ComponentSnapshot
  { pos :: Maybe Position } deriving (Show, Eq, Generic)

instance Serialise ComponentSnapshot
