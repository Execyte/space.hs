module Common.Networking.NetWorld
  (NetEntity(..),
   Dirty(..),
   Player(..),

   ComponentSnapshot(..),
   EntitySnapshot(..),
   WorldSnapshot(..),
  ) where

import Common.World
import Common.Networking

import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

import Apecs
import Apecs.Experimental.Reactive

import Data.Text(Text)

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

-- | This is a mapping between server and client entity IDs.
newtype NetEntity = NetEntity ServerEntityId
  deriving newtype (Eq, Ord, Show, Enum)
instance Component NetEntity where
  type Storage NetEntity = Reactive (EnumMap NetEntity) (Map NetEntity)

-- | This is used on the server to identify entities owned by the player, usually stuff they control. It just contains their username.
newtype Player = Player LoginName
  deriving Show
  deriving Generic
instance Component Player where
  type Storage Player = Map Player
instance Serialise Player

-- | This is used to mark an entity that it's components have changed and need to be reconciled to the clients. You should, for the most part, always add this if you want the clients to see the changes.
data Dirty = Dirty deriving Show
instance Component Dirty where
  type Storage Dirty = Map Dirty
