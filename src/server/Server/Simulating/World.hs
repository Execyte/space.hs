module Server.Simulating.World(World, System', initWorld, packComponentSnapshotFor, packWorld) where

import Apecs
import Control.Monad(foldM)
import Data.Functor((<&>))

import Common.Networking
import Common.Networking.NetWorld
import Common.World

foldMapM f = foldM (\acc x -> (<>) acc <$> f x) mempty

makeWorld "World" [''Player, ''Position, ''Facing, ''Dirty]

type System' a = System World a

packComponentSnapshotFor :: Entity -> System' ComponentSnapshot
packComponentSnapshotFor ent = ComponentSnapshot
  <$> (get ent :: System' (Maybe Position))
  <*> (get ent :: System' (Maybe Facing))

packWorld :: System' WorldSnapshot
packWorld = do
  ents <- collect \(Position _ _, ent@(Entity entId)) -> Just (ent, EntitySnapshot (ServerEntityId entId))
  entSnapshots <- foldMapM (\(ent, entitySnapshot) -> packComponentSnapshotFor ent <&> \componentSnapshot -> [entitySnapshot componentSnapshot]) ents
  pure $ WorldSnapshot entSnapshots
