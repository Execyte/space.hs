module Client.Game (runGame, handleEvent) where

import Apecs
import Linear
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Event
import Components

makeWorld "World" [''Camera, ''Client]

type System' a = System World a

step :: Float -> System' ()
step dT = pure ()

handleEvent :: Event -> System' ()
handleEvent _ = pure ()

runGame :: Float -> System' ()
runGame dT = set global $ Camera (V2 0.0 0.0)
