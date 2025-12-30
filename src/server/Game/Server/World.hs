module Game.Server.World(World, System', initWorld, packSnapshot, packWorld) where

import Apecs
import Game.Components
import Network.Apecs.Snapshot
import Types

makeWorld "World" [''Player, ''Position, ''Facing, ''Dirty]

type System' a = System World a

packSnapshot :: Entity -> System' ComponentSnapshot
packSnapshot ent = ComponentSnapshot
  <$> (get ent :: System' (Maybe Position))
  <*> (get ent :: System' (Maybe Facing))

packWorld :: System' WorldSnapshot
packWorld = do
  entsWithPos <- collect \(Position _ _, ent@(Entity entId)) -> Just ent
  
  snapshots <- entityListToSnapshot entsWithPos
  pure $ WorldSnapshot snapshots
    where
      entityListToSnapshot = mapM (\ent -> EntitySnapshot (ServerEntityId $ unEntity ent) <$> packSnapshot ent)
