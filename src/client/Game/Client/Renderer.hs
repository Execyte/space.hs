module Game.Client.Renderer (
  Renderer(..),
  Vertex(..),
  m44ToGL,
  loadImage
) where

import System.Exit
import System.IO
import Foreign
import Foreign.C
import Foreign.Storable

import Linear
import Codec.Picture
import Graphics.Rendering.OpenGL.GL qualified as GL
import SDL qualified

import Game.Client.Renderer.Shader (Shader)

data Vertex = Vertex (V2 Float) (V2 Float) (V4 Float)

-- TODO: this is garbage, find a better way to do this
instance Storable Vertex where
  {-# INLINE sizeOf #-}
  sizeOf _ =
      sizeOf (undefined :: V2 Float)
    + sizeOf (undefined :: V2 Float)
    + sizeOf (undefined :: V4 Float)
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: Float)
  {-# INLINE poke #-}
  poke ptr (Vertex position uv color) = do
    poke (castPtr ptr) position
    pokeByteOff ptr (sizeOf (undefined :: V2 Float)) uv
    pokeByteOff ptr (sizeOf (undefined :: V4 Float)) color
  {-# INLINE peek #-}
  peek ptr = Vertex
    <$> peek (castPtr ptr)
    <*> peekByteOff ptr (sizeOf (undefined :: V2 Float))
    <*> peekByteOff ptr (sizeOf (undefined :: V4 Float))

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
