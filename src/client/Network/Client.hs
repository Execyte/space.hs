module Network.Client(processEvent) where

import Apecs

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Network.Message
import Network.Apecs.Snapshot
import Network.Client.ConnectionStatus

import Game
import Game.Client
import Game.Client.World(withNetEntity)

-- | Here is where you process random data that the server sends to you.
-- The main cases here are component and world snapshots.
processEvent :: Client -> MessageFromServer -> IO ()
processEvent client (EntitySnapshotPacket (EntitySnapshot id snapshot)) =
  (atomically $ tryReadTMVar client.world) >>= \case
    Just world -> runWith world $ withNetEntity id
      \ent ->
        case snapshot.pos of
          Just (MkPosition pos) -> do
            lift $ putStrLn $ show pos
            modify ent \(MkPosition p) -> MkPosition pos
          Nothing -> pure ()
    Nothing -> pure ()
processEvent _ n = do
  putStrLn $ "Seen " <> show n
  pure ()

