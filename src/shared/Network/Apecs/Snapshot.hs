module Network.Apecs.Snapshot(ComponentSnapshot(..), EntitySnapshot(..), WorldSnapshot(..))where

import Game.Components
import GHC.Generics(Generic)
import Codec.Serialise(Serialise)
import Types
import Apecs

data ComponentSnapshot = ComponentSnapshot
  { pos :: Maybe Position
  , facing :: Maybe Facing
  } deriving (Show, Eq, Generic)

data EntitySnapshot = EntitySnapshot ServerEntityId ComponentSnapshot
  deriving (Show, Generic)

newtype WorldSnapshot = WorldSnapshot [EntitySnapshot]
  deriving (Show, Generic)

instance Serialise ComponentSnapshot
instance Serialise WorldSnapshot
instance Serialise EntitySnapshot

