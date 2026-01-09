module Common.World
  (-- Client-only Components
   Camera(..),
   Me(..),

   -- Shared components
   Position(..),
   Facing(..),

   -- Server-only components

   Intent(..),
   Direction(..),

   pattern Camera,
   pattern Position
  ) where

import GHC.Generics(Generic)

import Codec.Serialise(Serialise)

import Apecs

import Data.Semigroup
import Data.Monoid
import Data.Text(Text)

import Linear

-- | Intent is the data type that contains information about what the player is doing. Send these to the server to be able to interact with the world.
data Intent =
    Quit
  | Wait
  | Move Direction
  deriving (Eq, Show)
  deriving Generic

-- | This is the direction datatype used for movement, changing orientation, comparing among other things.
data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Eq, Show, Enum)
  deriving Generic

instance Serialise Direction
instance Serialise Intent

-- | TEMPORARY: remove this at some point and replace it for a newtype instead.
instance Serialise (V2 Float)

-- | Used to identify who you are as the client in game. Should only be used by the client.
data Me = Me deriving Show
instance Component Me where
  type Storage Me = Unique Me

-- | The position of the camera, this should only be used by the client.
newtype Camera = MkCamera (V2 Float) deriving Show
instance Component Camera where
  type Storage Camera = Global Camera
instance Semigroup Camera where (MkCamera p1) <> (MkCamera p2) = MkCamera $ p1 ^+^ p2
instance Monoid Camera where mempty = MkCamera $ V2 0 0
pattern Camera x y = MkCamera (V2 x y)

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
