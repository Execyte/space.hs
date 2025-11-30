module Client.UI.ConnectMenu(drawConnectMenu, newConnectMenu, ConnectMenu(..)) where

import qualified Client.ImGui as Im
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad(void, when, forever)
import Control.Monad.Extra(whenM)
import Control.Exception(try, SomeException(..))
import Network.Message
import Network.QUIC.Simple
import Network.ConnectionStatus
import Data.Text(Text, unpack, pack)
import System.Timeout(timeout)

data ConnectMenu = ConnectMenu
  { server_ip :: TVar Text
  , username :: TVar Text
  , password :: TVar Text
  }

newConnectMenu :: STM ConnectMenu
newConnectMenu = ConnectMenu <$> newTVar "127.0.0.1" <*> newTVar "" <*> newTVar ""

drawConnectMenu :: ConnectMenu -> TVar (ConnectionStatus ClientMessage ServerMessage) -> IO ()
drawConnectMenu ConnectMenu{username, password, server_ip} connInfo = do
  connStatus <- readTVarIO connInfo
  case connStatus of
    Connected _ -> pure ()
    _           -> Im.withWindowOpen "connect to server"
      case connStatus of
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

          whenM (Im.button "connect") do
            hostname <- readTVarIO server_ip
            name <- readTVarIO username
            pass <- readTVarIO password

            atomically $ writeTVar connInfo Connecting
            void $ forkIO $ timeout 5000000 (startClientSimple (unpack hostname) "2525") >>=
                \case
                  Just (stop, call) -> do
                    Ok _ <- call Hello

                    atomically $ writeTVar connInfo $
                      let
                        stopClient = do
                          atomically $ writeTVar connInfo $ Disconnected "manually disconnected from the server"
                          stop
                       in Connected (stopClient, call)

                    void $ forkIO $ forever do
                      threadDelay 2000000
                      timeout 1000000 (try $ call Ping) >>= \case
                        Just (Right Pong) -> pure ()
                        Just (Left (SomeException e)) -> do
                          atomically $ writeTVar connInfo $ Disconnected (pack $ show e)
                          stop
                          error "exception occured"
                        Nothing -> do
                          atomically $ writeTVar connInfo $ Disconnected "disconnected"
                          stop
                          error "disconnected"
                  Nothing -> atomically $ writeTVar connInfo $ Disconnected "unable to establish connection"

          Im.text str
          pure ()
        Connecting -> do
          hostname <- readTVarIO server_ip
          Im.text $ pack ("Connecting to " ++ (unpack hostname))
          
          t <- Im.getTime
          Im.setNextItemWidth 150
          Im.progressBar (-1 * (realToFrac t)) Nothing

