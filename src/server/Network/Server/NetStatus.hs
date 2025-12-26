module Network.Server.NetStatus(
  LoginName(..),
  ConnectionId(..),

  ConnectionStatus(..),
  Connection(..),

  NetStatus(..)) where

import Game.Intent(Intent)

import Network.Message
import Network.Types
import Network.Apecs.Snapshot

import GHC.Weak(Weak)

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.IntMap.Strict(IntMap)
import Data.HashMap.Strict(HashMap)
import Apecs(Entity)

newtype ConnectionId = ConnectionId Int deriving (Show, Eq, Ord)

-- | The connection status between the server and a client.
data ConnectionStatus =
    Connecting -- ^ Waiting for the login information to be sent.
  | LoggedIn LoginName -- ^ Logged in and active.
  | Disconnecting -- ^ Waiting to be properly removed from the game.
  deriving Show

-- | This is the definition of a connection.
data Connection = Connection
  { writeQueue :: TBQueue (ServerMessage MessageFromServer) -- ^ The write queue contains data that needs to be sent to the client. This is where you would write server to client messages.
  , connId :: Int
  , connStatus :: ConnectionStatus
  }

-- | This is where all information about the server's network is stored.
data NetStatus = NetStatus
  { players :: TVar (Map LoginName Entity) -- ^ Logged in players and their respective entities. Name mapped to the entity that they control.
  , logins :: TVar (Map ConnectionId LoginName) -- ^ Actively logged in players, connId mapped to the logged in name.
  , conns :: TVar (IntMap (Weak ThreadId, Connection)) -- ^ The active connections.
  , actions :: TBQueue (Entity, Intent) -- ^ A queue that contains actions of the player.
  , snapshots :: TVar (IntMap ComponentSnapshot) -- ^ A hashmap that contains last snapshots of every networked entity.
  }
