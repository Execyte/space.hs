module Game.Networking.Connection(ConnectionStatus(..)) where

import Data.Text(Text)

-- | This dictates the status of your connection to the server.
data ConnectionStatus q r =
    Disconnected Text -- ^ Disconnected followed by a reason.
  | Connecting -- ^ Downloading the world and having the credentials verified.
  | Connected {stop :: IO (), call :: (q -> IO r), cast :: (q -> IO ()), flush :: IO r} -- ^ Connected to the server, this stores `stop` `call` `cast` and `pollEvent` respectively.
