module Game.Client (Client(..), runGame, runDraw, initGame, step, draw) where

import SDL qualified
import Apecs
import Linear

import Game
import Game.Client.World
import Game.Client.Renderer(Renderer)

import Network.Message
import Network.Client.ConnectionStatus

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Data.IntMap.Strict(IntMap)

import Graphics.Rendering.OpenGL qualified as GL

-- | The client datatype. This stores pretty much everything relevant to the client, especially the world and connection status.
data Client = Client
  { world :: TMVar World
  , connStatus :: TVar (ConnectionStatus Message Message)
  , renderer :: Renderer
  }

-- | Initialize the world and run the initialization function.
initGame :: IO World
initGame = do
  world <- initWorld
  runWith world initialise
  pure world

-- | Step the world once. Here is where you should be putting most of the systems.
step :: Float -> System' ()
step dT = pure ()

-- | Draw the world to the screen.
draw :: System' ()
draw = pure ()

-- | Main function to set globals and other things when the world needs to be initialized.
initialise :: System' ()
initialise = set global $ Camera (V2 0.0 0.0)

runDraw :: World -> IO ()
runDraw world = runWith world draw

runGame :: Float -> World -> IO ()
runGame dT world = runWith world $ step dT

