module Game.Server.Simulation (World, System', initWorld, act) where

import Apecs
import Game

makeWorld "World" [''Player, ''Position]

type System' a = System World a

act :: Entity -> Intent -> System' ()
act ent (Move DOWN) = modify ent \(Position (V2 x y)) -> Position $ V2 x (y - 1)
act ent (Move UP) = modify ent \(Position (V2 x y)) -> Position $ V2 x (y + 1)
act ent (Move LEFT) = modify ent \(Position (V2 x y)) -> Position $ V2 (x - 1) y
act ent (Move RIGHT) = modify ent \(Position (V2 x y)) -> Position $ V2 (x + 1) y
act _ _ = pure ()

step :: Float -> System' ()
step dT = pure ()

