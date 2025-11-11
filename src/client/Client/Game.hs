module Client.Game (runGame, handleEvent) where

import Apecs
import Components
import Linear

import Event

makeWorld "World" [''Camera, ''Client]

type System' a = System World a

step :: Float -> System' ()
step dT = pure ()

handleEvent :: Event -> System' ()
handleEvent _ = pure ()

runGame :: Float -> System' ()
runGame dT = set global $ Camera (V2 0.0 0.0)
