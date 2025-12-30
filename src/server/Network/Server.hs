module Network.Server(handleConnecting, handleMessage) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.Message
import Network.Types
import Network.Server.NetStatus
import Network.Apecs.Snapshot

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict(IntMap)
import Data.IntMap.Strict qualified as IntMap

import Apecs

import Game.Components
import Game.Direction
import Game.Server
import Game.Server.World

loginInfo :: Map.Map LoginName Password
loginInfo = Map.fromList $ map (\(name, pass) -> (LoginName name, Password pass))
  [("test", "test"), ("test2", "test2")]

-- TODO: download the map too
handleCall :: Server -> NetStatus -> LoginName -> MessageFromClient -> IO (Maybe MessageFromServer)
handleCall server _ _ RequestWorldSnapshot = readTVarIO server.world >>= (runSystem $ (Just . WorldSnapshotPacket) <$> packWorld)
handleCall _ _ _ Ping = pure $ Just Pong
handleCall _ _ _ _ = pure Nothing

handleCast :: Server -> NetStatus -> LoginName -> MessageFromClient -> IO ()
handleCast server netstatus name (ActionPacket action) = do
  players' <- readTVarIO netstatus.players
  case Map.lookup name players' of
    Just ent -> atomically $ writeTBQueue netstatus.actions (ent, action)
    Nothing -> pure ()
handleCast _ _ _ _ = pure ()

checkPass :: Password -> Password -> Bool
checkPass = (==)

tryLogin :: Server -> NetStatus -> Connection -> LoginName -> Password -> IO (Maybe MessageFromServer)
tryLogin server netstatus conn name pass = do
  world <- readTVarIO server.world
  case Map.lookup name loginInfo of
    Just acctPass | checkPass acctPass pass -> do
      atomically $ modifyTVar' netstatus.logins \logins -> Map.insert (ConnectionId conn.connId) name logins
      ent <- tryMakeEntity world netstatus name
      (pure . Just . LoginStatusPacket) $ LoginSuccess $ unEntity ent
    _ -> pure Nothing
  where
    tryMakeEntity world netstatus name = do
      players <- readTVarIO netstatus.players
      case Map.lookup name players of
        Nothing -> do
          ent <- registerPlayer world name
          atomically $ modifyTVar' netstatus.players \players -> Map.insert name ent players
          pure ent
        Just ent -> pure ent

registerPlayer :: World -> LoginName -> IO Entity
registerPlayer world name = runWith world $ newEntity (Player name, Position 0 0, Facing DOWN, Dirty)

handleConnecting :: Server -> NetStatus -> Connection -> ClientMessage MessageFromClient -> IO (Connection, Maybe (ServerMessage MessageFromServer))
handleConnecting server netstatus conn (Call id (TryLogin name pass)) =
  tryLogin server netstatus conn name pass >>= \case
    Just reply -> pure (conn{connStatus = LoggedIn name}, (Just . Reply id) reply)
    Nothing -> do
      atomically $ writeTBQueue conn.writeQueue $ Reply id . LoginStatusPacket $ LoginFail "username or password is incorrect"
      error "disconnect"
      pure (conn, Nothing)
handleConnecting _ _ conn _ = pure (conn, Nothing)

handleMessage :: Server -> NetStatus -> Connection -> ClientMessage MessageFromClient -> IO (Connection, Maybe (ServerMessage MessageFromServer))
handleMessage server netstatus conn@Connection{connStatus=(LoggedIn name)} (Call id msg') = (conn,) . (Reply id <$>) <$> (handleCall server netstatus name msg')
handleMessage server netstatus conn@Connection{connStatus=(LoggedIn name)} (Cast msg') = do
  handleCast server netstatus name msg'
  pure (conn, Nothing)
handleMessage _ _ conn _ = pure (conn, Nothing)
