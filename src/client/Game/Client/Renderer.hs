module Game.Client.Renderer (
  Renderer,
  rendererWindow,
  rendererShader,
  rendererVertexBuffer,
  rendererElementBuffer,
  rendererVertexArray,
  rendererAtlas,
  rendererAtlasTexture,
  rendererTextures,
  newRenderer,
  
  atlasSize,

  loadTexture,
  getTexture,

  draw,

  m44ToGL,
  loadImage,

  module Game.Client.Renderer.Shader,
  module Game.Client.Renderer.Vertex
) where

import Data.Int
import Data.Word
import Data.Foldable (foldlM)
import Control.Monad.Primitive
import System.Exit
import System.IO
import Foreign

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Linear
import Codec.Picture
import Data.Atlas
import Data.StateVar
import Graphics.Rendering.OpenGL.GL qualified as GL
import SDL qualified
import Data.Vector.Storable qualified as Vector

import Game.Client.Renderer.Shader
import Game.Client.Renderer.Vertex

-- TODO: we shouldn't need access to the constructor and fields, make rendering possible without
-- having to access those
data Renderer = Renderer
  { rendererWindow :: SDL.Window
  , rendererShader :: Maybe Shader
  , rendererVertexBuffer :: GL.BufferObject
  , rendererElementBuffer :: GL.BufferObject
  , rendererVertexArray :: GL.VertexArrayObject
  , rendererAtlas :: Atlas (PrimState IO)
  , rendererAtlasTexture :: GL.TextureObject
  , rendererTextures :: HashMap String (GL.TextureObject, V4 Int)
  }

atlasSize :: Integral a => a
atlasSize = 1024

resetTexture :: GL.TextureObject -> IO GL.TextureObject
resetTexture texture = do
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture

  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

  GL.texImage2D
    GL.Texture2D
    GL.NoProxy
    0
    GL.RGBA'
    (GL.TextureSize2D atlasSize atlasSize)
    0
    $ GL.PixelData GL.RGBA GL.UnsignedByte nullPtr

  GL.textureBinding GL.Texture2D $= Nothing
  
  pure texture

newRenderer :: SDL.Window -> IO Renderer
newRenderer window = do
  [vertexBuffer, elementBuffer] <- GL.genObjectNames 2
  vertexArray <- GL.genObjectName
  texture <- GL.genObjectName >>= resetTexture
  
  atlas <- create atlasSize atlasSize

  pure $ Renderer {
      rendererWindow = window
    , rendererShader = Nothing
    , rendererVertexBuffer = vertexBuffer
    , rendererElementBuffer = elementBuffer
    , rendererVertexArray = vertexArray
    , rendererAtlas = atlas
    , rendererAtlasTexture = texture
    , rendererTextures = HashMap.empty
    }

loadTexture :: Renderer -> String -> FilePath -> IO Renderer
loadTexture renderer@Renderer{
    rendererAtlas = atlas
  , rendererAtlasTexture = texture
  , rendererTextures = textures
  } name file = do
  Image{
      imageWidth = width
    , imageHeight = height
    , imageData = pixels
    } <- loadImage file
  
  params@(texture', rect) <- tryPack atlas texture width height

  let
    V4
      (fromIntegral -> x) (fromIntegral -> y)
      (fromIntegral -> width') (fromIntegral -> height') = rect
  
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture'

  Vector.unsafeWith pixels $ \ptr ->
    GL.texSubImage2D
      GL.Texture2D
      0
      (GL.TexturePosition2D x y)
      (GL.TextureSize2D width' height')
      $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
  
  GL.textureBinding GL.Texture2D $= Nothing
  
  let textures' = HashMap.insert name params textures

  pure $ renderer {
      rendererAtlasTexture = texture'
    , rendererTextures = textures'
    }

  where
    tryPack atlas texture width height = do
      maybePos <- pack1 atlas $ Pt width height
      case maybePos of
        Just (Pt x y) -> pure (texture, V4 x y width height)
        Nothing -> do
          reset atlas
          texture' <- resetTexture texture
          tryPack atlas texture' width height -- TODO: this might cause an infinite loop with
                                              -- textures that don't fit into the atlas, which is
                                              -- not very likely to happen, but check the size
                                              -- beforehand just in case

getTexture :: Renderer -> String -> IO (GL.TextureObject, V4 Int)
getTexture Renderer{ rendererTextures = textures } name = do
  let maybeParams = HashMap.lookup name textures
  case maybeParams of
    Just x -> pure x
    Nothing -> do
      hPutStrLn stderr "wha??????? no tile???????????"
      exitFailure

draw :: Renderer -> String -> V2 Float -> V2 Float -> IO ()
draw renderer name
  (V2 x y)
  (V2 w h) = do
  (texture, rect) <- getTexture renderer name

  let vertices = mkVertices rect

  let
    vertexSize = sizeOf @Vertex undefined
    verticesSize = fromIntegral $ vertexSize * Vector.length vertices

  Vector.unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (verticesSize, ptr, GL.DynamicDraw)

  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture

  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GL.textureBinding GL.Texture2D $= Nothing

  where
    mkVertices (V4
      (fromIntegral -> tx) (fromIntegral -> ty)
      (fromIntegral -> tw) (fromIntegral -> th)) = Vector.fromList [
      Vertex (V2 x y) (V2 tx ty) (V4 1 1 1 1),
      Vertex (V2 (x + w) y) (V2 (tx + tw) ty) (V4 1 1 1 1),
      Vertex (V2 x (y + h)) (V2 tx (ty + th)) (V4 1 1 1 1),
      Vertex (V2 (x + w) (y + h)) (V2 (tx + tw) (ty + th)) (V4 1 1 1 1)
      ]

-- TODO: apparently M44 has Storable, so we can just pass its pointer to opengl without having to
-- use this function, so it appears it's useless (and i'm not a big fan of it too) and it shall be
-- removed from this module
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
