module Game.Components(
  ServerEntity, ClientEntity,
  Camera(..), Me(..), NetEntity(..), Player(..), Position(..),
) where

import GHC.Generics(Generic)

import Codec.Serialise(Serialise)

import Apecs
import Apecs.Experimental.Reactive

import Data.Semigroup
import Data.Monoid
import Data.Text(Text)

import Linear

type ServerEntity = Entity
type ClientEntity = Entity

-- Client to Server ID map
type ServerEntityId = Int

data Me = Me deriving Show
instance Component Me where
  type Storage Me = Unique Me

newtype Camera = Camera (V2 Float) deriving Show
instance Component Camera where
  type Storage Camera = Global Camera
instance Semigroup Camera where (Camera p1) <> (Camera p2) = Camera $ (p1 ^+^ p2)
instance Monoid Camera where mempty = Camera $ V2 0 0

newtype NetEntity = NetEntity ServerEntityId
  deriving newtype (Eq, Ord, Show, Enum)
instance Component NetEntity where
  type Storage NetEntity = Reactive (EnumMap NetEntity) (Map NetEntity)

newtype Position = Position (V2 Int) deriving Show
instance Component Position where
  type Storage Position = Map Position

newtype Player = Player Text
  deriving Show
  deriving Generic
instance Component Player where
  type Storage Player = Map Player
instance Serialise Player
