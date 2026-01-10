module Game.Simulating (Client(..), runGame, runDraw, initGame, step, draw) where

import SDL qualified

import Game.Rendering qualified as Renderer
import Game.Rendering (Renderer)
import Game.Networking
import Game.State
import Game.Simulating.World
import Game.TextureCollection

import Common.World
import Common.Networking
import Common.Networking.NetWorld
import Common.Networking.Message

import Apecs
import Apecs.Experimental.Reactive

import Linear

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

-- | Initialize the world and run the initialization function.
initGame :: IO World
initGame = do
  world <- initWorld
  runWith world initialise
  pure world

-- TODO: call step cus apparently it never gets called

-- | Step the world once. Here is where you should be putting most of the systems.
step :: Float -> System' ()
step dT = pure ()

-- | Draw the world to the screen.
draw :: Renderer -> System' ()
draw renderer = cmapM_ \(Position x y, Facing d) -> lift $
  Renderer.drawSprite renderer "human" (fromEnum d) (V2 x y) (V2 1 1)

-- | Main function to set globals and other things when the world needs to be initialized.
initialise :: System' ()
initialise = set global $ Camera 0.0 0.0

runDraw :: World -> Renderer -> IO ()
runDraw world renderer = runWith world $ draw renderer

runGame :: Float -> World -> IO ()
runGame dT world = runWith world $ step dT

