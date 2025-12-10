module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple qualified as QUIC
import Network.Snapshot
import Network.Server
import Network.Server.ConnectionStatus
import Network.Message

import Data.Text(Text, unpack, pack)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.IORef (newIORef, atomicModifyIORef')

import Codec.Serialise(Serialise)

import Apecs
import Game
import Game.Server
import Game.Server.Simulation

type Login = Text

loginInfo :: Map.Map Text Text
loginInfo = Map.fromList [("test", "test")]

handleCall :: Server -> ServerNetworkInfo -> Login -> Message -> IO (Maybe Message)
handleCall _ _ _ Ping = pure $ Just Pong
handleCall _ _ _ _ = pure Nothing

handleCast :: Server -> ServerNetworkInfo -> Login -> Message -> IO ()
handleCast server netinfo name (Action action) = do
  players' <- readTVarIO netinfo.players
  case Map.lookup name players' of
    Just ent -> atomically $ writeTBQueue netinfo.actions (ent, action)
    Nothing -> pure ()
handleCast _ _ _ _ = pure ()

registerPlayer :: World -> Text -> IO Entity
registerPlayer world name = runWith world $ newEntity (Player name, Position (V2 0 0))

checkPass :: Text -> Text -> Bool
checkPass = (==)

tryLogin :: Server -> ServerNetworkInfo -> Connection -> Login -> Text -> IO (Maybe Message)
tryLogin server netinfo conn name pass = do
  world <- readTVarIO server.world
  case Map.lookup name loginInfo of
    Just acctPass | checkPass acctPass pass -> do
      atomically $ modifyTVar' netinfo.logins \logins -> Map.insert conn.connId name logins
      ent <- tryMakeEntity world netinfo name
      pure $ Just (LoginSuccess (unEntity ent))
    _ -> pure Nothing
  where
    tryMakeEntity world netinfo name = do
      players <- readTVarIO netinfo.players
      case Map.lookup name players of
        Nothing -> do
          ent <- registerPlayer world name
          atomically $ modifyTVar' netinfo.players \players -> Map.insert name ent players
          pure $ ent
        Just ent ->
          pure $ ent

handleConnecting :: Server -> ServerNetworkInfo -> Connection -> ClientMessage Message -> IO (Connection, Maybe (ServerMessage Message))
handleConnecting server netinfo conn (Call id (TryLogin name pass)) = do
  let _str_name = unpack name
  reply <- tryLogin server netinfo conn name pass
  case reply of
    Just x -> do
      putStrLn $ "Player login: " <> _str_name 
      pure (conn{connStatus = LoggedIn name}, (Reply id) <$> (Just x))
    Nothing -> do
      atomically $ writeTBQueue conn.writeQueue (Reply id LoginFail)
      error "disconnect"
      pure (conn, Nothing)
handleConnecting _ _ conn _ = pure (conn, Nothing)

handleMessage :: Server -> ServerNetworkInfo -> Connection -> ClientMessage Message -> IO (Connection, Maybe (ServerMessage Message))
handleMessage server netinfo conn@Connection{connStatus=(LoggedIn name)} (Call id msg') = do
  reply <- handleCall server netinfo name msg'
  pure (conn, (Reply id) <$> reply)
handleMessage server netinfo conn@Connection{connStatus=(LoggedIn name)} (Cast msg') = do
  handleCast server netinfo name msg'
  pure (conn, Nothing)
handleMessage _ _ conn _ = pure (conn, Nothing)

main :: IO ()
main = do
  players <- newTVarIO Map.empty
  logins <- newTVarIO Map.empty
  conns <- newTVarIO mempty
  snapshots <- newTVarIO HashMap.empty
  actions <- newTBQueueIO 32
  connIds <- newIORef 0

  world' <- initWorld
  world <- newTVarIO world'
  
  let 
      server = Server { world = world }
      netinfo = ServerNetworkInfo {
        logins = logins
      , players = players
      , actions = actions
      , conns = conns
      , snapshots = snapshots
      }

  let
    handler conn msg =
      case conn.connStatus of
        Connecting -> handleConnecting server netinfo conn msg
        LoggedIn name -> handleMessage server netinfo conn msg
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
    world' <- readTVarIO world
    (ent, action) <- atomically $ readTBQueue actions
    Apecs.runWith world' $ do
      act ent action
      step (1/60)
      networkSystem netinfo

  QUIC.runServerStateful "127.0.0.1" 2525 (setup conns connIds) (teardown conns) handler
