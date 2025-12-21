module Game.Client.Renderer (
  Renderer(..),
  m44ToGL,
  loadImage,

  module Game.Client.Renderer.Shader,
  module Game.Client.Renderer.Vertex,
) where

import Data.Int
import Data.Word
import Data.Foldable (foldlM)
import System.Exit
import System.IO

import Linear
import Codec.Picture
import Data.StateVar
import Graphics.Rendering.OpenGL.GL qualified as GL
import SDL qualified

import Game.Client.Renderer.Shader
import Game.Client.Renderer.Vertex

data Renderer = Renderer
  { rendererWindow :: SDL.Window
  , rendererShader :: Shader
  , rendererTexture :: GL.TextureObject
  , rendererVertexBuffer :: GL.BufferObject
  , rendererVertexArray :: GL.VertexArrayObject
  }

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

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage file = do
  dynImage <- readImage file
  case dynImage of
    Left error -> do
      hPutStrLn stderr error
      exitFailure
    Right (ImageRGBA8 image) -> pure image
    Right image -> pure $ convertRGBA8 image
