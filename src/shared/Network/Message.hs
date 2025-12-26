module Network.Message(
  -- * Envelopes
  ClientMessage(..),
  ServerMessage(..),

  -- * Payload
  MessageFromClient(..),
  MessageFromServer(..)) where

import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)
import Game.Intent(Intent)
import Network.Types
import Network.Apecs.Snapshot

-- | These are the payloads that the server and client pass around.
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

data ClientMessage a = Call Int a | Cast a
  deriving (Show, Generic)

data ServerMessage a = Reply Int a | Event a
  deriving (Show, Generic)

instance Serialise a => Serialise (ClientMessage a)
instance Serialise a => Serialise (ServerMessage a)
