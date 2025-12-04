module Main (main) where

import Control.Monad (foldM_, unless, when)
import Control.Concurrent
import Control.Concurrent.STM 
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Data.Bits
import Data.Text(Text)
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Data.StateVar

import System.Exit
import System.IO

import Foreign

import Linear

import Codec.Picture

import Game.Client.Renderer qualified as Renderer
import Game.Client.Renderer.Shader qualified as Shader
import Game.Client.Renderer (Renderer(..))
import Im qualified
import DearImGui.OpenGL3
import DearImGui.Raw.IO qualified as ImIO
import DearImGui.SDL
import SDL qualified
import DearImGui.SDL.OpenGL
import Graphics.Rendering.OpenGL.GL qualified as GL

import Network.ConnectionStatus
import Network.Message

import Game
import Game.UI.ConnectMenu
import Game.Client
import Game.Client.World

-- TODO: implement tile layers
type Tile = Int

tiles :: [[Tile]]
tiles = [
    [0, 1, 0, 1, 0, 1, 0, 1],
    [1, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 1, 0, 1, 0, 1],
    [1, 0, 1, 0, 0, 1, 0, 0],
    [0, 1, 0, 1, 0, 1, 0, 1],
    [1, 0, 1, 0, 0, 1, 1, 0],
    [0, 1, 0, 1, 0, 1, 0, 1],
    [1, 0, 1, 0, 1, 0, 1, 0]
  ]

-- vertices :: Vector Float
-- vertices = Vector.fromList [
--   0, 0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0,
--   1, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
--   0, 1, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
--   1, 1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
--   ]

indices :: Vector Word32
indices = Vector.fromList [0, 1, 2, 1, 2, 3]

action :: Intent -> IO ()
action Quit = exitSuccess
action _ = pure ()

intentFromKey :: SDL.KeyboardEventData -> Maybe Intent
intentFromKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
intentFromKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Nothing
intentFromKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) = Just $
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeW -> Move UP
    SDL.KeycodeS -> Move DOWN
    SDL.KeycodeA -> Move LEFT
    SDL.KeycodeD -> Move RIGHT
    _ -> Wait

eventsToIntents :: [SDL.Event] -> [Intent]
eventsToIntents = mapMaybe (payloadToIntent . SDL.eventPayload)
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent (SDL.KeyboardEvent k)       = intentFromKey k
    payloadToIntent SDL.QuitEvent               = Just Quit
    payloadToIntent _                           = Nothing

drawTiles :: Renderer -> [[Tile]] -> IO ()
drawTiles renderer =
  foldM_ (\y row -> do
    foldM_ (\x tile -> do
      when (tile > 0) $ drawTile x y
      pure $ succ x
      ) 0 row
    pure $ succ y
    ) 0
  where
    makeVertices :: Int -> Int -> Vector Float
    makeVertices x y = Vector.fromList [
      fx, fy, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0,
      fx + 1, fy, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
      fx, fy + 1, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
      fx + 1, fy + 1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
      ]
      where
        (fx, fy) = (fromIntegral x, fromIntegral y)
    drawTile x y = do
      let vertices = makeVertices x y

      let
        floatSize = sizeOf (undefined :: Float)
        verticesSize = fromIntegral $ floatSize * Vector.length vertices
      
      Vector.unsafeWith vertices $ \ptr ->
        GL.bufferData GL.ArrayBuffer $= (verticesSize, ptr, GL.DynamicDraw)

      GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

render :: World -> Renderer -> IO ()
render world renderer = do
  let
    shader = Renderer.shader renderer
    vertexArray = Renderer.vertexArray renderer

  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just shader
  GL.bindVertexArrayObject $= Just vertexArray
  runDraw world
  drawTiles renderer tiles
  GL.bindVertexArrayObject $= Nothing

loop :: TMVar World -> Renderer -> IO () -> IO ()
loop worldTMVar renderer buildUI = do
  let
    window = Renderer.window renderer

  events <- pollEventsWithImGui
  let intents = eventsToIntents events

  let quit = Quit `elem` intents

  openGL3NewFrame
  sdl2NewFrame
  Im.newFrame

  buildUI

  let
    flags =
          Im.ImGuiWindowFlags_NoMove
      .|. Im.ImGuiWindowFlags_NoDecoration
      .|. Im.ImGuiWindowFlags_NoResize
      .|. Im.ImGuiWindowFlags_NoBackground

  let position = makeGettableStateVar . pure $ Im.ImVec2 10 10
  _ <- Im.setNextWindowPos position Im.ImGuiCond_Always Nothing

  let padding = makeGettableStateVar . pure $ Im.ImVec2 0 0
  Im.pushStyleVar Im.ImGuiStyleVar_WindowPadding padding

  Im.withWindowOpenFlags "overlay" flags $ do
    Im.text "TODO: use this overlay for something"

  Im.popStyleVar 1
 
  (atomically $ tryReadTMVar worldTMVar) >>= \case
    Just world -> render world renderer
    Nothing -> pure ()

  Im.render
  openGL3RenderDrawData =<< Im.getDrawData

  SDL.glSwapWindow window

  unless quit $ loop worldTMVar renderer buildUI

main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Space Station 15" SDL.defaultWindow {
    SDL.windowPosition = SDL.Centered,
    SDL.windowInitialSize = V2 800 600,
    SDL.windowResizable = True,
    SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  }

  image <- Renderer.loadImage "assets/ss15_icon.png"

  let
    width = fromIntegral (imageWidth image)
    height = fromIntegral (imageHeight image)
    pixels = imageData image

  icon <- SDL.createRGBSurface (V2 width height) SDL.ABGR8888

  SDL.lockSurface icon
  destination <- SDL.surfacePixels icon
  Vector.unsafeWith pixels $ \ptr ->
    copyBytes destination (castPtr ptr) (Vector.length pixels)
  SDL.unlockSurface icon

  SDL.setWindowIcon window icon

  gl <- SDL.glCreateContext window
  SDL.showWindow window

  im <- Im.createContext
  _ <- sdl2InitForOpenGL window gl
  _ <- openGL3Init

  ImIO.setIniFilename nullPtr

  GL.viewport $= (GL.Position 0 0, GL.Size 800 600)

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  GL.clearColor $= GL.Color4 0 0 0 0

  maybeShader <- Shader.fromFiles [
    (GL.VertexShader, "assets/vertex.glsl"),
    (GL.FragmentShader, "assets/fragment.glsl")
    ]

  shader <- case maybeShader of
    (Nothing, logs) -> do
      hPutStrLn stderr "THE SHADERS THEY'RE ON FIREEEEEEEEEEEEE!!!!!!!!!!!!!!!"
      hPutStrLn stderr logs
      exitFailure
    (Just shader, _) -> return shader

  texture <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture

  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

  image <- Renderer.loadImage "assets/tile.png"

  let
    width = fromIntegral (imageWidth image)
    height = fromIntegral (imageHeight image)
    pixels = imageData image

  Vector.unsafeWith pixels $ \ptr ->
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy 0
      GL.RGBA'
      (GL.TextureSize2D width height)
      0
      $ GL.PixelData GL.RGBA GL.UnsignedByte ptr

  Shader.setUniform shader "u_texture" (GL.TextureUnit 0)

  model <- Renderer.m44ToGL $ identity * V4 32 32 1 1
  Shader.setUniform shader "u_model" model

  projection <- Renderer.m44ToGL $ ortho 0 640 480 0 (-1) 1
  Shader.setUniform shader "u_projection" projection

  vertexArray <- GL.genObjectName
  vertexBuffer <- GL.genObjectName
  elementBuffer <- GL.genObjectName

  GL.bindVertexArrayObject $= Just vertexArray

  let
    floatSize = sizeOf (undefined :: Float)
    intSize = sizeOf (undefined :: Int)
    indicesSize = fromIntegral $ intSize * Vector.length indices
  
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  GL.bindBuffer GL.ElementArrayBuffer $= Just elementBuffer
  Vector.unsafeWith indices $ \ptr -> do
    GL.bufferData GL.ElementArrayBuffer $= (indicesSize, ptr, GL.StaticDraw)

  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 32 nullPtr)
  GL.vertexAttribPointer (GL.AttribLocation 1) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 32 $ plusPtr nullPtr $ floatSize * 2)
  GL.vertexAttribPointer (GL.AttribLocation 2) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 32 $ plusPtr nullPtr $ floatSize * 4)
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled

  GL.bindVertexArrayObject $= Nothing

  worldTMVar <- newEmptyTMVarIO
  connInfo <- newTVarIO $ Disconnected ""

  connectMenu <- atomically $ newConnectMenu
  let drawUI = drawConnectMenu worldTMVar connInfo connectMenu

  let renderer = Renderer {
    Renderer.window = window,
    Renderer.shader = shader,
    Renderer.texture = texture,
    Renderer.vertexBuffer = vertexBuffer,
    Renderer.vertexArray = vertexArray
  }

  void $ forkIO do
    world <- atomically $ readTMVar worldTMVar
    runGame (1/60) world

  loop worldTMVar renderer drawUI

  openGL3Shutdown
  sdl2Shutdown
  Im.destroyContext im

  SDL.destroyWindow window
  SDL.quit
