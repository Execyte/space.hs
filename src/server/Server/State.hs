module Server.State(Server(..), newServer, mkServer) where

import Server.Simulating.World
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.Text(Text)
import Data.Map(Map)
import Apecs(Entity)

-- | The server datatype that contains information that the server's side of the simulation needs to know.
data Server = Server
  { world :: TVar World
  }

newServer :: IO Server
newServer = do
  world' <- initWorld
  atomically $ mkServer world'

mkServer :: World -> STM Server
mkServer world = Server <$> newTVar world
