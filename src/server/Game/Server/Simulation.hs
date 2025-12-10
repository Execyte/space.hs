module Game.Server.Simulation (World, System', initWorld, step, act, networkSystem) where

import Apecs
import Game

import Data.Maybe
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Network.Message
import Network.Snapshot
import Network.Server
import Network.Server.ConnectionStatus

makeWorld "World" [''Player, ''Position, ''Dirty]

type System' a = System World a

act :: Entity -> Intent -> System' ()
act ent (Move DOWN) = modify ent \(Position (V2 x y)) -> (Position $ V2 x (y - 1), Dirty)
act ent (Move UP) = modify ent \(Position (V2 x y)) -> (Position $ V2 x (y + 1), Dirty)
act ent (Move LEFT) = modify ent \(Position (V2 x y)) -> (Position $ V2 (x - 1) y, Dirty)
act ent (Move RIGHT) = modify ent \(Position (V2 x y)) -> (Position $ V2 (x + 1) y, Dirty)
act _ _ = pure ()

step :: Float -> System' ()
step dT = pure ()

networkSystem :: ServerNetworkInfo -> System' ()
networkSystem netinfo = do
  dirties <- collect \(Dirty, Entity ent) -> Just ent
  forM_ dirties \ent_ -> do
    snapshots <- lift $ readTVarIO netinfo.snapshots
    let ent = Entity ent_
    modify ent \Dirty -> Not @Dirty
    case HashMap.lookup ent_ snapshots of
      Just snapshot -> do
        newPos <- get ent :: System' (Maybe Position)
        let newSnapshot = ComponentSnapshot{pos = either' newPos (pos snapshot)}

        if newSnapshot /= snapshot then
          lift $ do
            conns <- readTVarIO netinfo.conns
            forM_ conns $ \(_, (Connection{writeQueue})) -> atomically $ writeTBQueue writeQueue (Event $ ComponentSnapshotPacket ent_ newSnapshot)
            atomically $ modifyTVar' netinfo.snapshots (HashMap.insert ent_ newSnapshot)
        else
          pure ()
      Nothing -> do
        newPos <- get ent :: System' (Maybe Position)
        let snapshot = ComponentSnapshot{pos = newPos}
        lift $ do
          conns <- readTVarIO netinfo.conns
          forM_ conns $ \(_, (Connection{writeQueue})) -> atomically $ writeTBQueue writeQueue (Event $ ComponentSnapshotPacket ent_ snapshot)
          atomically $ modifyTVar' netinfo.snapshots (HashMap.insert ent_ snapshot)
  where
    either' Nothing b = b
    either' a Nothing = a
    either' a b = if b == a then b else a
