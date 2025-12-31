module Game.Server.World(World, System', initWorld, packComponentSnapshotFor, packWorld) where

import Apecs
import Game.Components
import Network.Apecs.Snapshot
import Control.Monad(foldM)
import Data.Functor((<&>))
import Types

foldMapM f = foldM (\acc x -> (<>) acc <$> f x) mempty

makeWorld "World" [''Player, ''Position, ''Facing, ''Dirty]

type System' a = System World a

packComponentSnapshotFor :: Entity -> System' ComponentSnapshot
packComponentSnapshotFor ent = ComponentSnapshot
  <$> (get ent :: System' (Maybe Position))
  <*> (get ent :: System' (Maybe Facing))

packWorld :: System' WorldSnapshot
packWorld = do
  ents <- collect \(Position _ _, ent@(Entity entId)) -> Just $ (ent, EntitySnapshot (ServerEntityId entId))
  entSnapshots <- foldMapM (\(ent, entitySnapshot) -> packComponentSnapshotFor ent <&> \componentSnapshot -> [entitySnapshot componentSnapshot]) ents
  pure $ WorldSnapshot entSnapshots
