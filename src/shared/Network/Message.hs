module Network.Message(ClientMessage(..), ServerMessage(..)) where

import Intent
import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

data ClientMessage = Ping | Hello | Bye
  deriving Generic

data ServerMessage = Pong | Ok Int
  deriving Generic

instance Serialise ClientMessage
instance Serialise ServerMessage

