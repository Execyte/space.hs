module Client.UI.ConnectMenu(drawConnectMenu, newConnectMenu, ConnectMenu(..)) where

import qualified Client.ImGui as Im
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad(void, when)
import Control.Monad.Extra(whenM)
import Network.UDP(UDPSocket)
import qualified Network.UDP as UDP(clientSocket, connected)
import Data.Text(Text, unpack)

data ConnectMenu = ConnectMenu
  { server_ip :: TVar Text
  , username :: TVar Text
  , password :: TVar Text
  }

newConnectMenu :: STM ConnectMenu
newConnectMenu = ConnectMenu <$> newTVar "localhost" <*> newTVar "" <*> newTVar ""

drawConnectMenu :: ConnectMenu -> TVar (Maybe UDPSocket) -> IO ()
drawConnectMenu ConnectMenu{username, password, server_ip} socket = do
  sock <- atomically $ readTVar socket
  when (not $ isConnected sock) $
    Im.withWindowOpen "connect to server" do
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

      whenM (Im.button "connect") connectHandler
      return ()
  where
    connectHandler = do
      hostname <- atomically $ readTVar server_ip
      name <- atomically $ readTVar username
      pass <- atomically $ readTVar password
      void $ forkIO do
        sock <- UDP.clientSocket (unpack hostname) "2020" True
        atomically $ writeTVar socket (Just sock)
    isConnected (Just sock) = UDP.connected sock
    isConnected Nothing = False
