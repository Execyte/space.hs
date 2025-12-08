module Network.Server.ConnectionStatus(ConnectionStatus(..), Connection(..)) where

import Data.Text(Text)

import Network.Message

import Control.Concurrent.STM.TBQueue

data ConnectionStatus =
    Connecting
  | LoggedIn Text
  | Disconnecting
  deriving Show

data Connection = Connection
  { writeQueue :: TBQueue (ServerMessage Message)
  , connId :: Int
  , connStatus :: ConnectionStatus
  }
