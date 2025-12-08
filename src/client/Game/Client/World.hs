module Game.Client.World(World, System', initWorld, withNetEntity)where

import Apecs
import Apecs.Experimental.Reactive
import Game.Components

makeWorld "World" [''Camera, ''Me, ''Position, ''NetEntity]

type System' a = System World a

withNetEntity f netEntity =
  withReactive (enumLookup netEntity) >>= \case
    [localEntity] -> f localEntity
    _ -> pure ()
