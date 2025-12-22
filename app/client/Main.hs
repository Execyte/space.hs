module Main (main) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Text (Text)
import Control.Monad
import System.Exit
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Foreign hiding (void)

import Linear
import Codec.Picture
import Data.StateVar
import DearImGui.OpenGL3
import DearImGui.Raw.IO qualified as ImIO
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.Rendering.OpenGL.GL qualified as GL
import SDL qualified

import Game.Direction
import Game.Intent
import Game.Client
import Game.Client.World
import Game.UI.ConnectMenu
import Network.Message
import Network.Client.ConnectionStatus
import Game.Client.Renderer qualified as Renderer
import Game.Client.Renderer (Renderer(..), Vertex(..))
import Im qualified

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

indices :: Vector Word32
indices = Vector.fromList [0, 1, 2, 1, 2, 3]

action :: Client -> Intent -> IO ()
action Client{connStatus} Quit = exitSuccess
action Client{connStatus} x = do
  status <- readTVarIO connStatus
  case status of
    Connected (_, _, event, _) -> event (Action x)
    _ -> pure ()
action _ _ = pure ()

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
-- drawTiles _ _ = pure () -- we kinda broke it now because of the TEXTURE UPDATE™, so right now we
--                         -- don't run it so that it doesn't freak out and combust
  where
    makeVertices :: Int -> Int -> Vector Vertex
    makeVertices x y = Vector.fromList [
      Vertex (V2 fx fy) (V2 0 0) (V4 1 1 1 1),
      Vertex (V2 (fx + 1) fy) (V2 32 0) (V4 1 1 1 1),
      Vertex (V2 fx (fy + 1)) (V2 0 32) (V4 1 1 1 1),
      Vertex (V2 (fx + 1) (fy + 1)) (V2 32 32) (V4 1 1 1 1)
      ]
      where
        (fx, fy) = (fromIntegral x, fromIntegral y)
    drawTile x y = do
      let vertices = makeVertices x y

      let
        vertexSize = sizeOf (undefined :: Vertex)
        verticesSize = fromIntegral $ vertexSize * Vector.length vertices

      Vector.unsafeWith vertices $ \ptr ->
        GL.bufferData GL.ArrayBuffer $= (verticesSize, ptr, GL.DynamicDraw)

      GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

renderGame :: World -> Renderer -> IO ()
renderGame world renderer = do
  runDraw world

  ((/ 1000) . fromIntegral -> seconds) <- SDL.ticks
  (V2 (fromIntegral -> w) (fromIntegral -> h)) <- get $ SDL.windowSize renderer.rendererWindow

  let
    x = cos (seconds * 3) * 3
    y = sin (seconds * 3) * 3
    w' = w / 32
    h' = h / 32
    cx = (w' - 1) / 2
    cy = (h' - 1) / 2
    x' = cx - x
    y' = cy - y

  Renderer.draw renderer "tile" (V2 x' y') (V2 1 1)

loop :: Client -> IO () -> IO ()
loop client@Client{world, renderer} buildUI = forever do
  let
    window = renderer.rendererWindow

  events <- pollEventsWithImGui
  let intents = eventsToIntents events

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

  let
    shader = renderer.rendererShader
    vertexArray = renderer.rendererVertexArray

  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= shader -- shader might be Nothing, in which case some stupid utter buffoon
                              -- broke the part where the shader is initialized and loaded into the
                              -- renderer, causing the program to combust spontaneously during
                              -- rendering
                              -- i can't be bothered to check if it's Nothing life is too short for
                              -- that kinda shit
  GL.bindVertexArrayObject $= Just vertexArray

  (atomically $ tryReadTMVar world) >>= \case
    Just world' -> renderGame world' renderer
    Nothing -> pure ()
  GL.bindVertexArrayObject $= Nothing

  Im.render
  openGL3RenderDrawData =<< Im.getDrawData

  SDL.glSwapWindow window
  forM_ intents (action client)

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

  renderer <- Renderer.newRenderer window

  im <- Im.createContext
  _ <- sdl2InitForOpenGL window gl
  _ <- openGL3Init

  ImIO.setIniFilename nullPtr

  GL.viewport $= (GL.Position 0 0, GL.Size 800 600)

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  GL.clearColor $= GL.Color4 0 0 0 0

  maybeShader <- Renderer.shaderFromFiles [
    (GL.VertexShader, "assets/vertex.glsl"),
    (GL.FragmentShader, "assets/fragment.glsl")
    ]

  shader <- case maybeShader of
    (Nothing, logs) -> do
      hPutStrLn stderr "THE SHADERS THEY'RE ON FIREEEEEEEEEEEEE!!!!!!!!!!!!!!!"
      hPutStrLn stderr logs
      exitFailure
    (Just shader, _) -> return shader

  -- ok uh haskell doesn't like when i shadow this name so i guess i'll just put an apostrophe here
  -- it looks worse but we cannot have good-looking code in this world
  let renderer' = renderer { rendererShader = Just shader }

  -- texture <- GL.genObjectName
  -- GL.activeTexture $= GL.TextureUnit 0
  -- GL.textureBinding GL.Texture2D $= Just texture

  -- GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

  -- image <- Renderer.loadImage "assets/tile.png"

  -- let
  --   width = fromIntegral (imageWidth image)
  --   height = fromIntegral (imageHeight image)
  --   pixels = imageData image

  -- Vector.unsafeWith pixels $ \ptr ->
  --   GL.texImage2D
  --     GL.Texture2D
  --     GL.NoProxy 0
  --     GL.RGBA'
  --     (GL.TextureSize2D width height)
  --     0
  --     $ GL.PixelData GL.RGBA GL.UnsignedByte ptr

  -- Renderer.setUniform shader "u_texture" (GL.TextureUnit 0)

  -- please don't make me do this haskell
  renderer'' <- Renderer.loadTexture renderer' "tile" "assets/tile.png"

  model <- Renderer.m44ToGL $ identity * V4 32 32 1 1
  Renderer.setUniform shader "u_model" model

  projection <- Renderer.m44ToGL $ ortho 0 800 600 0 (-1) 1
  Renderer.setUniform shader "u_projection" projection

  Renderer.setUniform @Word32 shader "u_atlas_size" Renderer.atlasSize

  let
    vertexBuffer = renderer''.rendererVertexBuffer
    elementBuffer = renderer''.rendererElementBuffer
    vertexArray = renderer''.rendererVertexArray

  GL.bindVertexArrayObject $= Just vertexArray

  let
    -- floatSize = sizeOf (undefined :: Float)
    intSize = sizeOf (undefined :: Int)
    indicesSize = fromIntegral $ intSize * Vector.length indices

  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  GL.bindBuffer GL.ElementArrayBuffer $= Just elementBuffer
  Vector.unsafeWith indices $ \ptr -> do
    GL.bufferData GL.ElementArrayBuffer $= (indicesSize, ptr, GL.StaticDraw)

  -- GL.vertexAttribPointer (GL.AttribLocation 0) $=
  --   (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 32 nullPtr)
  -- GL.vertexAttribPointer (GL.AttribLocation 1) $=
  --   (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 32 $ plusPtr nullPtr $ floatSize * 2)
  -- GL.vertexAttribPointer (GL.AttribLocation 2) $=
  --   (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 32 $ plusPtr nullPtr $ floatSize * 4)
  -- GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  -- GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  -- GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled

  Renderer.setVertexAttribs @Vertex undefined vertexArray

  GL.bindVertexArrayObject $= Nothing

  worldTMVar <- newEmptyTMVarIO
  connInfo <- newTVarIO $ Disconnected ""

  let client = Client {
    world = worldTMVar,
    connStatus = connInfo,
    renderer = renderer''
  }

  connectMenu <- atomically $ newConnectMenu
  let drawUI = drawConnectMenu client connectMenu

  void $ forkIO do
    world <- atomically $ readTMVar worldTMVar
    runGame (1/60) world

  loop client drawUI

  openGL3Shutdown
  sdl2Shutdown
  Im.destroyContext im

  SDL.destroyWindow window
  SDL.quit
