module Main (main) where

import Control.Monad (when, unless)
import System.Exit
import System.IO
import Data.ByteString (ByteString)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign.Ptr

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

import Client.Renderer (Renderer(..))
import qualified Client.Renderer as Renderer
import qualified Client.Renderer.Shader as Shader

{-
  HELPERS
-}
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

loadImage :: FilePath -> IO (Either String (Image PixelRGBA8))
loadImage file = do
  dynImage <- readImage file
  case dynImage of
    Left error -> pure (Left error)
    Right (ImageRGBA8 image) -> pure (Right image)
    Right image -> pure (Right $ convertRGBA8 image)

{-
  DATA
-}
vertices :: Vector Float 
vertices = Vector.fromList [
  0, 0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.25,
  256, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.25,
  0, 256, 0.0, 1.0, 1.0, 1.0, 1.0, 0.25,
  256, 0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.25,
  0, 256, 0.0, 1.0, 1.0, 1.0, 1.0, 0.25,
  256, 256, 1.0, 1.0, 1.0, 1.0, 1.0, 0.25
  ]

{-
  MAIN LOOP
-}
loop :: Renderer -> IO ()
loop renderer = do
  let
    window = Renderer.window renderer
    shader = Renderer.shader renderer

  events <- pollEventsWithImGui
  let quit = SDL.QuitEvent `elem` map SDL.eventPayload events

  openGL3NewFrame
  sdl2NewFrame
  Im.newFrame

  Im.withMainMenuBarOpen $ do
    Im.withMenuOpen "File" $ do
      Im.menuItem "New map..." >>= \clicked ->
        when clicked $ putStrLn "unimplemented"
      Im.menuItem "Open" >>= \clicked ->
        when clicked $ putStrLn "unimplemented"

  Im.withWindowOpen "the space station 15" $ do
    Im.text "the space station 15 is real"

  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just shader
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

  window <- SDL.createWindow "Space Station 15 Editor" SDL.defaultWindow {
    SDL.windowPosition = SDL.Centered,
    SDL.windowInitialSize = V2 640 480,
    SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  }
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

  eitherImage <- loadImage "assets/ss15_full_title.png"
  image <- case eitherImage of
    Left error -> do
      hPutStrLn stderr error
      exitFailure
    Right image -> return image

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

  projection <- m44ToGL $ ortho 0 640 480 0 (-1) 1
  Shader.setUniform shader "u_projection" projection

  GL.currentProgram $= Nothing

  let renderer = Renderer {
    Renderer.window = window,
    Renderer.shader = shader,
    Renderer.texture = texture
  }

  loop renderer

  openGL3Shutdown
  sdl2Shutdown
  Im.destroyContext im

  SDL.destroyWindow window
  SDL.quit