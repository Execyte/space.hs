module Main (main) where

import Data.Maybe
import Control.Monad (unless)
import System.Exit
import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign

import Linear
import Codec.Picture
import Data.StateVar
import qualified DearImGui as Im
import DearImGui.OpenGL3
import qualified DearImGui.Raw.IO as ImIO
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified SDL

import qualified Direction
import Intent (Intent)
import qualified Intent
import Client.Renderer (Renderer(..))
import qualified Client.Renderer as Renderer
import qualified Client.Renderer.Shader as Shader

vertices :: Vector Float 
vertices = Vector.fromList [
  0, 0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0,
  400, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
  0, 210, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  400, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
  0, 210, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  400, 210, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
  ]

action :: Intent -> IO ()
action Intent.Quit = exitSuccess
action _ = pure ()

intentFromKey :: SDL.KeyboardEventData -> Maybe Intent
intentFromKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
intentFromKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Nothing
intentFromKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) = Just $
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Intent.Quit
    SDL.KeycodeW -> Intent.Move Direction.Up
    SDL.KeycodeS -> Intent.Move Direction.Down
    SDL.KeycodeA -> Intent.Move Direction.Left
    SDL.KeycodeD -> Intent.Move Direction.Right
    _ -> Intent.Wait

eventsToIntents :: [SDL.Event] -> [Intent]
eventsToIntents = mapMaybe (payloadToIntent . SDL.eventPayload)
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent (SDL.KeyboardEvent k)       = intentFromKey k
    payloadToIntent SDL.QuitEvent               = Just Intent.Quit
    payloadToIntent _                           = Nothing

loop :: Renderer -> IO ()
loop renderer = do
  let
    window = Renderer.window renderer
    shader = Renderer.shader renderer
    vertexArray = Renderer.vertexArray renderer

  events <- pollEventsWithImGui
  let intents = eventsToIntents events

  let quit = Intent.Quit `elem` intents

  openGL3NewFrame
  sdl2NewFrame
  Im.newFrame

  Im.showDemoWindow
  Im.showMetricsWindow

  Im.withWindowOpen "the space station 15" $ do
    Im.text "the space station 15 is real"

  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just shader
  GL.bindVertexArrayObject $= Just vertexArray
  GL.drawArrays GL.Triangles 0 6
  GL.bindVertexArrayObject $= Nothing

  Im.render
  openGL3RenderDrawData =<< Im.getDrawData

  SDL.glSwapWindow window

  unless quit $ loop renderer

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
  
  vertexArray <- GL.genObjectName
  vertexBuffer <- GL.genObjectName
  
  GL.bindVertexArrayObject $= Just vertexArray
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  let
    floatSize = fromIntegral $ sizeOf (undefined :: Float)
    verticesSize = fromIntegral $ floatSize * Vector.length vertices
  Vector.unsafeWith vertices $ \ptr -> do
    GL.bufferData GL.ArrayBuffer $= (verticesSize, ptr, GL.StaticDraw)
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

  image <- Renderer.loadImage "assets/ss15_full_title.png"

  let
    width = fromIntegral (imageWidth image)
    height = fromIntegral (imageHeight image)
    pixels = imageData image
  
  Vector.unsafeWith pixels $ \ptr -> do
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy 0
      GL.RGBA'
      (GL.TextureSize2D width height)
      0
      $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
  
  Shader.setUniform shader "u_texture" (GL.TextureUnit 0)

  projection <- Renderer.m44ToGL $ ortho 0 640 480 0 (-1) 1
  Shader.setUniform shader "u_projection" projection

  let renderer = Renderer {
    Renderer.window = window,
    Renderer.shader = shader,
    Renderer.texture = texture,
    Renderer.vertexBuffer = vertexBuffer,
    Renderer.vertexArray = vertexArray
  }

  loop renderer

  openGL3Shutdown
  sdl2Shutdown
  Im.destroyContext im

  SDL.destroyWindow window
  SDL.quit