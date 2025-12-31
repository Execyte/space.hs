module Game.Components(
  -- Client-only Components
  Camera(..),
  NetEntity(..),
  Me(..),

  -- Shared components
  Player(..),
  Position(..),
  Facing(..),

  -- Server-only components
  Dirty(..),

  pattern Camera,
  pattern Position
) where

import GHC.Generics(Generic)

import Codec.Serialise(Serialise)

import Apecs
import Apecs.Experimental.Reactive

import Data.Semigroup
import Data.Monoid
import Data.Text(Text)

import Network.Types
import Types

import Codec.Serialise(Serialise)

import GHC.Generics(Generic)

import Linear

import Game.Direction

-- | TEMPORARY: remove this at some point and replace it for a newtype instead.
instance Serialise (V2 Float)

-- | Used to identify who you are as the client in game. Easier to query for this.
data Me = Me deriving Show
instance Component Me where
  type Storage Me = Unique Me

-- | The position of the camera, this should only be available to the client.
newtype Camera = MkCamera (V2 Float) deriving Show
instance Component Camera where
  type Storage Camera = Global Camera
instance Semigroup Camera where (MkCamera p1) <> (MkCamera p2) = MkCamera $ (p1 ^+^ p2)
instance Monoid Camera where mempty = MkCamera $ V2 0 0

pattern Camera x y = MkCamera (V2 x y)

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

-- | This is the position of any entity in the game world. Keep it as Int for servers, but floats for clients (for animation mostly.)
newtype Position = MkPosition (V2 Float)
  deriving (Show, Eq)
  deriving Generic
instance Component Position where
  type Storage Position = Map Position
instance Serialise Position
pattern Position x y = MkPosition (V2 x y)

-- | This is used for entities that can face in multiple directions.
newtype Facing = Facing Direction
  deriving (Show, Eq)
  deriving Generic
instance Component Facing where
  type Storage Facing = Map Facing
instance Serialise Facing

-- | This is used to mark an entity that it's components have changed and need to be reconciled to the clients. You should, for the most part, always add this if you want the clients to see the changes.
data Dirty = Dirty deriving Show
instance Component Dirty where
  type Storage Dirty = Map Dirty
