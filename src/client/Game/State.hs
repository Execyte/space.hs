module Game.State(Client(..)) where

import Game.Rendering
import Game.Textures
import Game.Networking.Connection
import Game.Simulating.World

import Common.World
import Common.Networking
import Common.Networking.Message

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

-- | The client datatype. This stores pretty much everything relevant to the client, especially the world and connection status.
data Client = Client
  { world :: TMVar World
  , connStatus :: TVar (ConnectionStatus MessageFromClient MessageFromServer)
  , renderer :: Renderer
  }
