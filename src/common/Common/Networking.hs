module Common.Networking
  (LoginStatus(..),
   LoginName(..),
   Password(..),

   ServerEntityId(..),
   ClientEntityId(..),

   ServerEntity(..),
   ClientEntity(..),

   ClientMessage(..),
   ServerMessage(..)
  ) where

import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

import Apecs(Entity)

import Data.Text(Text)

data LoginStatus =
    LoginSuccess Int -- ^ successful login with the entity ID 
  | LoginFail Text -- ^ failed login with error message
  deriving (Show, Generic)

newtype LoginName = LoginName Text
  deriving (Show, Eq, Ord, Generic)

newtype Password = Password Text
  deriving (Show, Eq, Ord, Generic)

newtype ServerEntityId = ServerEntityId Int deriving (Show, Eq, Ord, Enum, Generic)
newtype ClientEntityId = ClientEntityId Int deriving (Show, Eq, Ord, Enum, Generic)

newtype ServerEntity = ServerEntity Entity deriving (Show, Eq, Ord)
newtype ClientEntity = ClientEntity Entity deriving (Show, Eq, Ord)

data ClientMessage a = Call Int a | Cast a
  deriving (Show, Generic)

data ServerMessage a = Reply Int a | Event a
  deriving (Show, Generic)

instance Serialise a => Serialise (ClientMessage a)
instance Serialise a => Serialise (ServerMessage a)

instance Serialise ServerEntityId
instance Serialise ClientEntityId

instance Serialise LoginName
instance Serialise Password
instance Serialise LoginStatus
