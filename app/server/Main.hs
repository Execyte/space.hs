module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple qualified as QUIC
import Network.Server.ConnectionStatus
import Network.Message

import Data.Text(Text, unpack, pack)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
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

handleCall :: Server -> Login -> Message -> IO (Maybe Message)
handleCall _ _ Ping = pure $ Just Pong
handleCall _ _ _ = pure Nothing

handleCast :: Server -> Login -> Message -> IO ()
handleCast server name (Action x) = do
  players' <- readTVarIO server.players
  case Map.lookup name players' of
    Just ent -> runWith server.world $ act ent x
    Nothing -> pure ()
handleCast _ _ _ = pure ()

registerPlayer :: World -> Text -> IO Entity
registerPlayer world name = runWith world $ newEntity (Player name, Position (V2 0 0))

checkPass :: Text -> Text -> Bool
checkPass = (==)

tryLogin :: Server -> Connection -> Login -> Text -> IO (Maybe Message)
tryLogin server conn name pass =
  case Map.lookup name loginInfo of
    Just acctPass | checkPass acctPass pass -> do
      ent <- registerPlayer server.world name
      atomically $ modifyTVar' server.players \players -> Map.insert name ent players
      atomically $ modifyTVar' server.logins \logins -> Map.insert conn.connId name logins
      pure $ Just (LoginSuccess (unEntity ent))
    _ -> pure Nothing

handleConnecting :: Server -> Connection -> ClientMessage Message -> IO (Connection, Maybe (ServerMessage Message))
handleConnecting server conn (Call id (TryLogin name pass)) = do
  let _str_name = unpack name
  putStrLn $ "Someone trying to login as " <> _str_name
  reply <- tryLogin server conn name pass
  case reply of
    Just x -> do
      putStrLn $ _str_name <> ": Login success"
      pure (conn{connStatus = LoggedIn name}, (Reply id) <$> (Just x))
    Nothing -> do
      atomically $ writeTBQueue conn.writeQueue (Reply id LoginFail)
      putStrLn $ _str_name <> ": Login failed: invalid password"
      error "disconnect"
      pure (conn, Nothing)
handleConnecting _ conn _ = pure (conn, Nothing)

handleMessage :: Server -> Connection -> ClientMessage Message -> IO (Connection, Maybe (ServerMessage Message))
handleMessage server conn@Connection{connStatus=(LoggedIn name)} (Call id msg') = do
  reply <- handleCall server name msg'
  pure (conn, (Reply id) <$> reply)
handleMessage server conn@Connection{connStatus=(LoggedIn name)} (Cast msg') = do
  handleCast server name msg'
  pure (conn, Nothing)
handleMessage _ conn _ = pure (conn, Nothing)

main :: IO ()
main = do
  players <- newTVarIO Map.empty
  logins <- newTVarIO Map.empty
  conns <- newTVarIO mempty
  --actions <- newTBQueueIO 32
  connIds <- newIORef 0

  world <- initWorld
  
  let server = Server {
    players = players,
    logins = logins,
    world = world
  }

  let
    handler conn msg =
      case conn.connStatus of
        Connecting -> handleConnecting server conn msg
        LoggedIn name -> handleMessage server conn msg
        Disconnecting -> pure (conn, Nothing)

  QUIC.runServerStateful "127.0.0.1" 2525 (setup conns connIds) (teardown conns) handler
    where
      setup conns counter _conn writeQ = do
        connId <- atomicModifyIORef' counter \old -> (old + 1, old)
        me <- myThreadId >>= mkWeakThreadId
        atomically $ modifyTVar' conns $ IntMap.insert connId (me, _conn)
        let conn = Connection {connId = connId, writeQueue = writeQ, connStatus = Connecting}
        pure conn

      teardown conns _conn conn =
        atomically $ modifyTVar' conns $ IntMap.delete conn.connId
