module Network.Client(processEvent) where

import Apecs

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Network.Message
import Network.Snapshot
import Network.Client.ConnectionStatus

import Game
import Game.Client
import Game.Client.World(withNetEntity)

processEvent :: Client -> Message -> IO ()
processEvent client (ComponentSnapshotPacket id snapshot) =
  (atomically $ tryReadTMVar client.world) >>= \case
    Just world -> runWith world $ withNetEntity id
      \ent ->
        case snapshot.pos of
          Just (Position pos) -> do
            lift $ putStrLn $ show pos
            modify ent \(Position p) -> Position pos
          Nothing -> pure ()
    Nothing -> pure ()
processEvent _ n = do
  putStrLn $ "Seen " <> show n
  pure ()

