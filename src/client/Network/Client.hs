module Network.Client(startClient) where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Network.Message
import Network.QUIC.Simple qualified as QUIC
import Network.Socket (HostName)

import Control.Concurrent.Async (async, cancel, link)

import Data.IntMap.Strict qualified as IntMap
import Data.IORef (newIORef, atomicModifyIORef')

import Codec.Serialise(Serialise)

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
