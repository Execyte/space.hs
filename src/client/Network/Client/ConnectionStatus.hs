module Network.Client.ConnectionStatus(ConnectionStatus(..)) where

import Data.Text(Text)

data ConnectionStatus q r =
    Disconnected Text
  | Connecting
  | Connected (IO (), (q -> IO r), (q -> IO ()), IO r)

