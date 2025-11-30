module Network.ConnectionStatus(ConnectionStatus(..)) where

import Codec.Serialise(Serialise)
import Data.Text(Text)
import GHC.Generics(Generic)

data ConnectionStatus q r =
    Disconnected Text
  | Connecting
  | Connected (IO (), (q -> IO r))
