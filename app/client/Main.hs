module Main (main) where

import System.IO
import System.Exit
import Control.Monad (unless)
import Data.Maybe
import Data.ByteString (ByteString)
import Data.Vector.Storable (Vector)
import qualified Data.ByteString as ByteString
import qualified Data.Vector.Storable as Vector
import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL (($=))
import Intent (Intent)
import qualified Intent
import qualified Direction
import Linear
import Client.Renderer (Renderer)
import qualified Client.Renderer as Renderer

{-
  RENDERING
-}
vertices :: Vector Float 
vertices = Vector.fromList [
  -0.5, -0.5,
  0.5, -0.5,
  -0.5, 0.5,
  0.5, 0.5
  ]

vertexSource :: ByteString
vertexSource = "#version 330 core\n\
  \layout (location = 0) in vec2 position;\n\
  \void main()\n\
  \{\n\
  \   gl_Position = vec4(position, 0.0, 1.0);\n\
  \}\0"

fragmentSource :: ByteString
fragmentSource = "#version 330 core\n\
  \out vec4 FragColor;\n\
  \void main()\n\
  \{\n\
  \   gl_FragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);\n\
  \}\0"

createShader :: Int -> GL.ShaderType -> ByteString -> IO GL.Shader
createShader id type' source = do
  shader <- GL.createShader type'
  GL.shaderSourceBS shader $= source
  GL.compileShader shader

  success <- GL.get $ GL.compileStatus shader
  unless success $ do
    hPutStrLn stderr ("funny error in shader " ++ show id)
    exitFailure
  
  return shader

{-
  INPUT
-}
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
    program = Renderer.program renderer

  events <- SDL.pollEvents
  let intents = eventsToIntents events

  let quit = elem Intent.Quit intents

  GL.clear [GL.ColorBuffer]
  GL.currentProgram $= Just program
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  Vector.unsafeWith vertices $ \pointer ->
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 pointer)
  GL.drawArrays GL.TriangleStrip 0 4
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
  SDL.glSwapWindow window

  unless quit (loop renderer)

{-
  MAIN
-}
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Space Station 15" SDL.defaultWindow {
      SDL.windowInitialSize = V2 640 480,
      SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  }
  _ <- SDL.glCreateContext window
  SDL.showWindow window

  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral 640) (fromIntegral 480))
  GL.clearColor $= GL.Color4 0 0 0 0
  
  vertexShader <- createShader 1 GL.VertexShader vertexSource
  fragmentShader <- createShader 2 GL.FragmentShader fragmentSource

  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragmentShader
  GL.attribLocation program "location" $= GL.AttribLocation 0
  GL.linkProgram program
  GL.validateProgram program
  linked <- GL.get $ GL.linkStatus program
  valid <- GL.get $ GL.validateStatus program
  unless (linked && valid) $ do
    hPutStrLn stderr "stupid program error!!!"
    exitFailure
  
  let renderer = Renderer.Renderer {
    Renderer.window = window,
    Renderer.program = program
  }

  loop renderer

  SDL.destroyWindow window
  SDL.quit
