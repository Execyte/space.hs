module Game.Simulating.World (World, System', initWorld) where

import Apecs
import Apecs.Experimental.Reactive

import Common.World
import Common.Networking.NetWorld

makeWorld "World" [''Camera, ''Me, ''Position, ''Facing, ''NetEntity]

type System' a = System World a
