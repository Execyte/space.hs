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

data Client = Client
  { world :: TMVar World
  , textureMaps :: TVar [GL.TextureObject]
  , sprites :: TVar (IntMap (Int, V4 Int))
  , connStatus :: TVar (ConnectionStatus Message Message)
  , renderer :: Renderer
  }

initGame :: IO World
initGame = do
  world <- initWorld
  runWith world initialise
  pure world

initialise :: System' ()
initialise = set global $ Camera (V2 0.0 0.0)

step :: Float -> System' ()
step dT = pure ()

draw :: System' ()
draw = pure ()

runDraw :: World -> IO ()
runDraw world = runWith world draw

runGame :: Float -> World -> IO ()
runGame dT world = runWith world $ step dT

