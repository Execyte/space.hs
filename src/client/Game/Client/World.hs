module Game.Client.World(World, System', initWorld, withNetEntity, getNetEntity)where

import Apecs
import Apecs.Experimental.Reactive
import Game.Components
import Types

makeWorld "World" [''Camera, ''Me, ''Position, ''Facing, ''NetEntity]

type System' a = System World a

-- | Used to look up entities by whatever their server id counterpart is.
withNetEntity :: ServerEntityId -> (Entity -> System' ()) -> System' ()
withNetEntity netEntity f =
  withReactive (enumLookup (NetEntity netEntity)) >>= \case
    [localEntity] -> f localEntity
    _ -> pure ()

getNetEntity :: ServerEntityId -> System' (Maybe Entity)
getNetEntity netEntity =
  withReactive (enumLookup (NetEntity netEntity)) >>= \case
    [localEntity] -> pure $ Just localEntity
    _ -> pure $ Nothing
