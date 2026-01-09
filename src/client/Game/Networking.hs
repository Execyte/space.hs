module Game.Networking(processEvent, startClient, withNetEntity, getNetEntity, ConnectionStatus(..)) where

import Apecs
import Apecs.Experimental.Reactive

import Control.Monad
import Control.Monad.Extra
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Common.World
import Common.Networking
import Common.Networking.NetWorld
import Common.Networking.Message

import Game.State
import Game.Simulating.World
import Game.Networking.Connection

import Network.QUIC.Simple qualified as QUIC
import Network.Socket (HostName)

import Control.Concurrent.Async (async, cancel, link)

import Data.IntMap.Strict qualified as IntMap
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Text(Text)

import Codec.Serialise(Serialise)

withNetEntity :: ServerEntityId -> (Entity -> System' ()) -> System' ()
withNetEntity netEntity f =
  withReactive (enumLookup (NetEntity netEntity)) >>= \case
    [localEntity] -> f localEntity
    _ -> pure ()

getNetEntity :: ServerEntityId -> System' (Maybe Entity)
getNetEntity netEntity =
  withReactive (enumLookup (NetEntity netEntity)) >>= \case
    [localEntity] -> pure $ Just localEntity
    _ -> pure Nothing

processEntitySnapshot :: EntitySnapshot -> System' ()
processEntitySnapshot (EntitySnapshot id snapshot) = do
  ent <- getNetEntity id >>= (maybe (newEntity (NetEntity id)) pure)
  set ent (snapshot.pos, snapshot.facing)

processEvent' :: Client -> MessageFromServer -> World -> IO ()
processEvent' client (EntitySnapshotPacket entSnapshot) = runSystem $ processEntitySnapshot entSnapshot
processEvent' client (WorldSnapshotPacket (WorldSnapshot xs)) = runSystem $ mapM_ processEntitySnapshot xs

processEvent :: Client -> MessageFromServer -> IO ()
processEvent client evt = atomically (tryReadTMVar client.world) >>= maybe (pure ()) (processEvent' client evt)

-- | Start the process of connecting to the server, setting up the appropriate queues and managing events/replies from the server.
startClient :: (Serialise q, Serialise r)
            => HostName
            -> String
            -> IO (IO (),
                   q -> IO r,
                   q -> IO (),
                   IO r)
startClient hostname port = do
  (client, _conn, (writeQ, readQ)) <- QUIC.startClientAsync hostname port

  let stop = cancel client

  events <- newTBQueueIO 16
  calls <- newTVarIO mempty
  counter <- newIORef 0

  let cast q = atomically $ writeTBQueue writeQ $ Cast q
  let pollEvent = atomically $ readTBQueue events

  void $ async do
      link client
      forever do
        atomically (readTBQueue readQ) >>= \case
          Event e ->
            atomically $ writeTBQueue events e
          Reply callId r ->
            atomically $ modifyTVar' calls $ IntMap.insert callId r

  counter <- newIORef 0
  let
    call q = do
      callId <- atomicModifyIORef' counter \old -> (old + 1, old)
      atomically $ writeTBQueue writeQ $ Call callId q
      replyVar <- newTVarIO undefined
      let
        peek = \case
          Nothing -> retry
          Just r -> Nothing <$ writeTVar replyVar r
      atomically $ readTVar calls >>= IntMap.alterF peek callId >>= writeTVar calls

      readTVarIO replyVar

  pure (stop, call, cast, pollEvent)
