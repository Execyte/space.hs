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
import Client.Renderer.Shader (Shader(..))
import qualified Client.Renderer.Shader as Shader

-- TODO: move to separate file
m44ToGL :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
m44ToGL m = GL.newMatrix GL.ColumnMajor [
  e0, e4, e8, eC,
  e1, e5, e9, eD,
  e2, e6, eA, eE,
  e3, e7, eB, eF
  ]
  where
    V4
      (V4 e0 e1 e2 e3)
      (V4 e4 e5 e6 e7)
      (V4 e8 e9 eA eB)
      (V4 eC eD eE eF) = m

{-
  RENDERING
-}
vertices :: Vector Float 
vertices = Vector.fromList [
  0, 0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0,
  256, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
  0, 256, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  256, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
  0, 256, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  256, 256, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
  ]

vertexSource :: ByteString
vertexSource = "#version 330 core\n\
  \uniform mat4 u_projection;\n\
  \layout (location = 0) in vec2 position;\n\
  \layout (location = 1) in vec2 uv;\n\
  \layout (location = 2) in vec4 color;\n\
  \out vec2 frag_uv;\n\
  \out vec4 frag_color;\n\
  \void main()\n\
  \{\n\
  \    gl_Position = u_projection * vec4(position, 0, 1);\n\
  \    frag_uv = uv;\n\
  \    frag_color = color;\n\
  \}\0"

fragmentSource :: ByteString
fragmentSource = "#version 330 core\n\
  \uniform sampler2D u_texture;\n\
  \in vec2 frag_uv;\n\
  \in vec4 frag_color;\n\
  \out vec4 gl_FragColor;\n\
  \void main()\n\
  \{\n\
  \    gl_FragColor = texture(u_texture, frag_uv) * frag_color;\n\
  \}\0"

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

  events <- pollEventsWithImGui
  let intents = eventsToIntents events

  let quit = Intent.Quit `elem` intents

  openGL3NewFrame
  sdl2NewFrame
  Im.newFrame

  Im.withWindowOpen "the space station 15" $ do
    Im.text "the space station 15 is real"

  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just program
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
  Vector.unsafeWith vertices $ \pointer -> do
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 32 pointer)
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 32 $ pointer `plusPtr` 8)
    GL.vertexAttribPointer (GL.AttribLocation 2) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 32 $ pointer `plusPtr` 16)
  GL.drawArrays GL.Triangles 0 6
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled
  GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Disabled

  Im.render
  openGL3RenderDrawData =<< Im.getDrawData

  SDL.glSwapWindow window

  unless quit $ loop renderer

{-
  MAIN
-}
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Space Station 15" SDL.defaultWindow {
    SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  }
  
  image <- readImage "assets/ss15_icon.png"
  case image of
    Left e -> putStrLn e
    Right (ImageRGBA8 image) -> do
      icon <- SDL.createRGBSurface
        (V2 (fromIntegral $ imageWidth image) (fromIntegral $ imageHeight image))
        SDL.ABGR8888
      
      SDL.lockSurface icon
      let vector = imageData image
      pixels <- SDL.surfacePixels icon
      Vector.unsafeWith vector $ \raw ->
        copyBytes pixels (castPtr raw) (Vector.length vector)
      SDL.unlockSurface icon

      SDL.setWindowIcon window icon
    Right _ -> putStrLn "what"

  gl <- SDL.glCreateContext window
  SDL.showWindow window

  im <- Im.createContext
  _ <- sdl2InitForOpenGL window gl
  _ <- openGL3Init

  ImIO.setIniFilename nullPtr

  GL.viewport $= (GL.Position 0 0, GL.Size 640 480)

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  
  GL.clearColor $= GL.Color4 0 0 0 0
  
  maybeShader <- Shader.fromByteStrings [
    (GL.VertexShader, vertexSource),
    (GL.FragmentShader, fragmentSource)
    ]

  shader <- case maybeShader of
    (Nothing, logs) -> do
      hPutStrLn stderr "THE SHADERS THEY'RE ON FIREEEEEEEEEEEEE!!!!!!!!!!!!!!!"
      hPutStrLn stderr logs
      exitFailure
    (Just shader, _) -> return shader
  
  let program = Shader.program shader

  texture <- GL.genObjectName
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture

  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

  dynImage <- readImage "assets/ss15_full_title.png"
  case dynImage of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right (ImageRGBA8 image) -> do
      let width = fromIntegral $ imageWidth image
      let height = fromIntegral $ imageHeight image
      let pixels = imageData image
      Vector.unsafeWith pixels $ \ptr ->
        GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA' (GL.TextureSize2D width height) 0 $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
    Right _ -> do
      hPutStrLn stderr "fuck you"
      exitFailure
  
  GL.currentProgram $= Just program

  uniform <- GL.uniformLocation program "u_texture"
  GL.uniform uniform $= GL.TextureUnit 0

  projection <- m44ToGL $ ortho 0 640 480 0 (-1) 1

  uniform <- GL.uniformLocation program "u_projection"
  GL.uniform uniform $= projection

  let renderer = Renderer.Renderer {
    Renderer.window = window,
    Renderer.program = program,
    Renderer.texture = texture
  }

  loop renderer

  openGL3Shutdown
  sdl2Shutdown
  Im.destroyContext im

  SDL.destroyWindow window
  SDL.quit
