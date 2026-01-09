module Common.Networking.Message(MessageFromClient(..), MessageFromServer(..)) where

import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

import Apecs(Entity)

import Data.Text(Text)

import Common.Networking
import Common.Networking.NetWorld
import Common.World

data MessageFromClient =
    Ping
  | ActionPacket Intent
  | TryLogin LoginName Password
  | RequestWorldSnapshot
  deriving (Show, Generic)
data MessageFromServer =
    Pong
  | LoginStatusPacket LoginStatus
  | EntitySnapshotPacket EntitySnapshot
  | WorldSnapshotPacket WorldSnapshot
  deriving (Show, Generic)

instance Serialise MessageFromClient
instance Serialise MessageFromServer
