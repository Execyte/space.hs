module Game.Server.Simulation (World, System', initWorld, step, act, packWorld, sendUpdatesToClients) where

import Apecs

import Game
import Game.Server.World

import Data.Maybe
import Data.IntMap.Strict(IntMap)
import Data.IntMap.Strict qualified as IntMap

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Network.Message
import Network.Server
import Network.Server.NetStatus
import Network.Apecs.Snapshot

import Types

-- | Process the player's actions.
act :: Entity -> Intent -> System' ()
act ent (Move UP) = modify ent \(Position x y) -> (Position x (y - 1), Dirty)
act ent (Move DOWN) = modify ent \(Position x y) -> (Position x (y + 1), Dirty)
act ent (Move LEFT) = modify ent \(Position x y) -> (Position (x - 1) y, Dirty)
act ent (Move RIGHT) = modify ent \(Position x y) -> (Position (x + 1) y, Dirty)
act _ _ = pure ()

-- | Step the world once.
step :: Float -> System' ()
step dT = pure ()

sendUpdatesToClients :: NetStatus -> System' ()
sendUpdatesToClients netstatus = do
  dirties <- collect \(Dirty, Entity entId) -> Just (Entity entId, entId)
  forM_ dirties \(ent, entId) -> do
    snapshots <- lift $ readTVarIO netstatus.snapshots

    modify ent \Dirty -> Not @Dirty
    snapshot <- packSnapshot ent

    -- TODO: replace with stm-containers 
    case IntMap.lookup entId snapshots of
      Just oldSnapshot | snapshot == oldSnapshot -> pure ()
      _ -> lift do
          conns <- readTVarIO netstatus.conns
          forM_ conns \(_, Connection{writeQueue}) -> atomically $ writeTBQueue writeQueue $ Event . EntitySnapshotPacket $ EntitySnapshot (ServerEntityId entId) snapshot
          atomically $ modifyTVar' netstatus.snapshots $ IntMap.insert entId snapshot
