module Game.UI.ConnectMenu(drawConnectMenu, newConnectMenu, ConnectMenu(..)) where

import Codec.Serialise (Serialise)

import Im qualified

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.Extra(whenM)

import Control.Exception(try, SomeException(..))

import Network.Message
import Network.Client qualified as Client
import Network.Client.ConnectionStatus

import Apecs

import Linear

import Game.Client
import Game.Client.World
import Game.Components

import Data.Text(Text, unpack, pack)

import System.Timeout(timeout)

data ConnectMenu = ConnectMenu
  { server_ip :: TVar Text
  , username :: TVar Text
  , password :: TVar Text
  }

newConnectMenu :: STM ConnectMenu
newConnectMenu = ConnectMenu <$> newTVar "127.0.0.1" <*> newTVar "" <*> newTVar ""

drawConnectMenu :: Client -> ConnectMenu -> IO ()
drawConnectMenu client ConnectMenu{server_ip, username, password} = do
  connStatus <- readTVarIO client.connStatus
  case connStatus of
    Connected _ -> pure ()
    _           -> Im.withWindowOpen "connect to server" case connStatus of
        Disconnected str -> do
          Im.text "Server IP:"
          Im.sameLine

          Im.setNextItemWidth 150
          void $ Im.inputText "##server_ip" server_ip 32

          Im.text "Username:"
          Im.sameLine

          Im.setNextItemWidth 150
          void $ Im.inputText "##toconnect_username" username 128

          Im.text "Password:"
          Im.sameLine

          Im.setNextItemWidth 150
          void $ Im.inputText "##toconnect_password" password 128

          whenM (Im.button "connect") $ connectHandler server_ip username password client.world client.connStatus

          Im.text str
          pure ()
            where
              tryLogin name pass world connStatus = do
                (Connected (_, call, _, _)) <- readTVarIO connStatus
                void $ call (TryLogin name pass) >>= \case
                  (LoginSuccess e) -> do
                    putStrLn $ "YAY! LOGIN SUCCESS! MY ENTITY NUMBER IS " <> (show e)
                    world' <- initGame
                    void $ runWith world' $ newEntity (Me, Position (V2 0 0), NetEntity e)
                    atomically $ writeTMVar world world'
                  LoginFail -> do
                    atomically $ writeTVar connStatus $ Disconnected "server reported login fail"
                    error "disconnect"

              pingLoop connStatus = do
                (Connected (stop, call, _, _)) <- readTVarIO connStatus
                void $ forkIO $ forever do
                  threadDelay 2000000
                  timeout 1000000 (try $ call Ping) >>= \case
                    Just (Right Pong) -> pure ()
                    Just (Left (SomeException e)) -> do
                      stop
                      atomically $ writeTVar connStatus $ Disconnected (pack $ show e)
                      error "exception occured"
                    Nothing -> do
                      stop
                      atomically $ writeTVar connStatus $ Disconnected "disconnected"
                      error "disconnected"

              tryStartClient hostname port f = do
                void $ forkIO $ timeout 5000000 (Client.startClient hostname "2525") >>= \case
                  Just (stop, call, cast, pollEvent) -> f $ Connected (stop, call, cast, pollEvent)
                  Nothing -> f $ Disconnected "no response from server"

              connectHandler server_ip username password world connStatus = do
                hostname <- readTVarIO server_ip
                name <- readTVarIO username
                pass <- readTVarIO password
                tryStartClient (unpack hostname) "2525" \case
                  Connected (stop_, call, cast, pollEvent) -> do
                    let
                      stop = do
                        void $ atomically $ tryTakeTMVar world
                        stop_

                      stopManual = do
                        stop
                        atomically $ writeTVar connStatus $ Disconnected "manually disconnected from the server"
                    atomically $ writeTVar connStatus $ Connected (stopManual, call, cast, pollEvent)

                    tryLogin name pass world connStatus
                    pingLoop connStatus
                  Disconnected str -> atomically $ writeTVar connStatus $ Disconnected str

        Connecting -> do
          hostname <- readTVarIO server_ip
          Im.text $ pack ("Connecting to " ++ (unpack hostname))
          
          t <- Im.getTime
          Im.setNextItemWidth 150
          Im.progressBar (-1 * (realToFrac t)) Nothing

