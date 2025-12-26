module Types where

import GHC.Generics(Generic)
import Codec.Serialise(Serialise)

import Apecs(Entity)

newtype ServerEntityId = ServerEntityId Int deriving (Show, Eq, Ord, Enum, Generic)
newtype ClientEntityId = ClientEntityId Int deriving (Show, Eq, Ord, Enum, Generic)

newtype ServerEntity = ServerEntity Entity deriving (Show, Eq, Ord)
newtype ClientEntity = ClientEntity Entity deriving (Show, Eq, Ord)

instance Serialise ServerEntityId
instance Serialise ClientEntityId
