module Game.Client (Client(..), runGame, runDraw, initGame, step, draw) where

import SDL qualified
import Apecs
import Linear

import Game
import Game.Client.World
import Game.Client.Renderer(Renderer)
import Game.Client.Renderer qualified as Renderer

import Network.Message
import Network.Client.ConnectionStatus

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Data.IntMap.Strict(IntMap)

-- | The client datatype. This stores pretty much everything relevant to the client, especially the world and connection status.
data Client = Client
  { world :: TMVar World
  , connStatus :: TVar (ConnectionStatus MessageFromClient MessageFromServer)
  , renderer :: Renderer
  }

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
draw renderer = cmapM_ \(Position x y) -> lift $
  Renderer.drawSprite renderer "human" 0 (V2 x y) (V2 1 1)

-- | Main function to set globals and other things when the world needs to be initialized.
initialise :: System' ()
initialise = set global $ Camera 0.0 0.0

runDraw :: World -> Renderer -> IO ()
runDraw world renderer = runWith world $ draw renderer

runGame :: Float -> World -> IO ()
runGame dT world = runWith world $ step dT

