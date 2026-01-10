module Server (runServer) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple qualified as QUIC

import Data.Text(Text, unpack, pack)
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Foldable(for_)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

import Codec.Serialise(Serialise)

import Apecs

import Server.State
import Server.Networking
import Server.Networking.Status
import Server.Simulating
import Server.Simulating.World

import Common.Networking
import Common.Networking.Message

import GHC.Weak (Weak)

type NetStatusConns = TVar (IntMap (Weak ThreadId, Connection))

setup :: NetStatusConns -> IORef Int -> c -> TBQueue (ServerMessage MessageFromServer) -> IO Connection
setup conns counter _conn writeQ = do
  connId <- atomicModifyIORef' counter \old -> (old + 1, old)
  me <- myThreadId >>= mkWeakThreadId
  let conn = Connection {connId = connId, writeQueue = writeQ, connStatus = Connecting}
  atomically $ modifyTVar' conns $ IntMap.insert connId (me, conn)
  pure conn

handler :: Server -> NetStatus -> Connection -> ClientMessage MessageFromClient -> IO (Connection, Maybe (ServerMessage MessageFromServer))
handler server netstatus conn msg =
  case conn.connStatus of
    Connecting -> handleConnecting server netstatus conn msg
    LoggedIn name -> handleMessage server netstatus conn msg
    Disconnecting -> pure (conn, Nothing)

teardown :: NetStatusConns -> c -> Connection -> IO ()
teardown conns _conn conn =
  atomically $ modifyTVar' conns $ IntMap.delete conn.connId

processGameStuff :: Server -> NetStatus -> IO ()
processGameStuff server netstatus =
  void $ forkIO $ forever do
    world' <- readTVarIO server.svWorld
    mAction <- atomically $ tryReadTBQueue netstatus.actions
    Apecs.runWith world' $ do
      for_ mAction $ uncurry act
      step (1/60)
      sendUpdatesToClients netstatus

-- TODO: fix memory leak relating to connected users not being removed after a certain time of not pinging
runServer = do
  connIds <- newIORef 0

  server <- newServer
  netstatus <- newNetStatus

  processGameStuff server netstatus

  QUIC.runServerStateful "127.0.0.1" 57355 (setup netstatus.conns connIds) (teardown netstatus.conns) (handler server netstatus)
