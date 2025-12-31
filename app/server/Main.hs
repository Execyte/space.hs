module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple qualified as QUIC
import Network.Server
import Network.Server.NetStatus
import Network.Message

import Data.Text(Text, unpack, pack)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Foldable(for_)
import Data.IORef (newIORef, atomicModifyIORef')

import Codec.Serialise(Serialise)

import Apecs

import Game.Server
import Game.Server.Simulation
import Game.Server.World

-- TODO: fix memory leak relating to connected users not being removed after a certain time of not pinging
main :: IO ()
main = do
  connIds <- newIORef 0

  server <- newServer
  netstatus <- newNetStatus

  let
    handler conn msg =
      case conn.connStatus of
        Connecting -> handleConnecting server netstatus conn msg
        LoggedIn name -> handleMessage server netstatus conn msg
        Disconnecting -> pure (conn, Nothing)
    setup conns counter _conn writeQ = do
      connId <- atomicModifyIORef' counter \old -> (old + 1, old)
      me <- myThreadId >>= mkWeakThreadId
      let conn = Connection {connId = connId, writeQueue = writeQ, connStatus = Connecting}
      atomically $ modifyTVar' conns $ IntMap.insert connId (me, conn)
      pure conn

    teardown conns _conn conn =
      atomically $ modifyTVar' conns $ IntMap.delete conn.connId

  void $ forkIO $ forever do
    world' <- readTVarIO server.world
    mAction <- atomically $ tryReadTBQueue netstatus.actions
    Apecs.runWith world' $ do
      for_ mAction \(ent, action) -> act ent action
      step (1/60)
      sendUpdatesToClients netstatus

  QUIC.runServerStateful "127.0.0.1" 57355 (setup netstatus.conns connIds) (teardown netstatus.conns) handler
