module Network.Client(processEvent) where

import Apecs

import Control.Monad
import Control.Monad.Extra
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Network.Message
import Network.Apecs.Snapshot
import Network.Client.ConnectionStatus

import Game
import Game.Client
import Game.Client.World

processEntitySnapshot :: EntitySnapshot -> System' ()
processEntitySnapshot (EntitySnapshot id snapshot) = do
  ent <- getNetEntity id >>= (maybe (newEntity (NetEntity id)) $ pure)
  set ent (snapshot.pos, snapshot.facing)

-- | Here is where you process random data that the server sends to you.
processEvent' :: Client -> MessageFromServer -> World -> IO ()
processEvent' client (EntitySnapshotPacket entSnapshot) = runSystem $ processEntitySnapshot entSnapshot
processEvent' client (WorldSnapshotPacket (WorldSnapshot xs)) = runSystem $ sequence_ $ map processEntitySnapshot xs

processEvent :: Client -> MessageFromServer -> IO ()
processEvent client evt = (atomically $ tryReadTMVar client.world) >>= maybe (pure ()) (processEvent' client evt)

