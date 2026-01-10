module Server.State(Server(..), newServer, mkServer) where

import Server.Simulating.World

import Common.World.Tiles

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.Text(Text)
import Data.Map(Map)
import Apecs(Entity)

import Linear

data GameMap a = GameMap [[a]]

-- | The server datatype that contains information that the server's side of the simulation needs to know.
data Server = Server
  { svWorld :: TVar World
  , svMap :: TVar (GameMap (Layers Tile))
  }

newServer :: IO Server
newServer = do
  world' <- initWorld
  atomically $ mkServer world'

mkServer :: World -> STM Server
mkServer world = Server <$> newTVar world <*> newTVar (GameMap [])
