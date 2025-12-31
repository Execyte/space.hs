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
  loadSpritesheet,
  
  getTexture,
  getSpritesheet,
  
  drawTexture,
  drawSprite,

  updateViewport,
  m44ToGL,
  loadImage,

  module Game.Client.Renderer.Shader,
  module Game.Client.Renderer.Vertex,
  module Game.Client.Renderer.Spritesheet
) where

import Data.Int
import Data.Word
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Aeson
import Data.Foldable (foldlM)
import Data.Vector ((!?))
import Control.Monad.Primitive
import System.Exit
import System.IO
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Foreign

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Linear
import Codec.Picture
import Data.Atlas
import Data.StateVar
import Graphics.Rendering.OpenGL.GL qualified as GL
import SDL qualified

import Game.Client.Renderer.Shader
import Game.Client.Renderer.Vertex
import Game.Client.Renderer.Spritesheet

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
  , rendererTextures :: HashMap Text (GL.TextureObject, V4 Int)
  , rendererSprites :: HashMap Text Spritesheet
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
    , rendererSprites = HashMap.empty
    }

loadTexture :: Renderer -> Text -> FilePath -> Maybe FilePath -> IO Renderer
loadTexture renderer@Renderer{
    rendererAtlas = atlas
  , rendererAtlasTexture = texture
  , rendererTextures = textures
  } name textureFile maybeSpritesheetFile = do
  Image{
      imageWidth = width
    , imageHeight = height
    , imageData = pixels
    } <- loadImage textureFile

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

  let renderer' = renderer {
      rendererAtlasTexture = texture'
    , rendererTextures = textures'
    }
  
  case maybeSpritesheetFile of
    Just spritesheetFile -> loadSpritesheet renderer' name spritesheetFile
    Nothing -> pure renderer'

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

loadSpritesheet :: Renderer -> Text -> FilePath -> IO Renderer
loadSpritesheet renderer@Renderer{
    rendererSprites = sprites
  } name file = do
  maybeSpritesheet <- decode @Spritesheet <$> ByteString.readFile file
  spritesheet <- case maybeSpritesheet of
    Just x -> pure x
    Nothing -> do
      hPutStrLn stderr "spritesheet parse error!!!!!!"
      exitFailure

  let sprites' = HashMap.insert name spritesheet sprites

  pure $ renderer {
      rendererSprites = sprites'
    }

getTexture :: Renderer -> Text -> IO (GL.TextureObject, V4 Int)
getTexture Renderer{ rendererTextures = textures } name = do
  let maybeParams = HashMap.lookup name textures
  case maybeParams of
    Just x -> pure x
    Nothing -> do
      hPutStrLn stderr "wha??????? no texture???????????"
      exitFailure

getSpritesheet :: Renderer -> Text -> IO Spritesheet
getSpritesheet renderer@Renderer{ rendererSprites = sprites } name = do
  let maybeSpritesheet = HashMap.lookup name sprites
  case maybeSpritesheet of
    Just x -> pure x
    Nothing -> do
      hPutStrLn stderr "wha??????? no spritesheet???????????"
      exitFailure

makeQuad :: V4 Float -> V4 Float -> Vector Vertex
makeQuad (V4 x y w h) (V4 tx ty tw th) =
  Vector.fromList [
    Vertex (V2 x y) (V2 tx ty) (V4 1 1 1 1),
    Vertex (V2 (x + w) y) (V2 (tx + tw) ty) (V4 1 1 1 1),
    Vertex (V2 x (y + h)) (V2 tx (ty + th)) (V4 1 1 1 1),
    Vertex (V2 (x + w) (y + h)) (V2 (tx + tw) (ty + th)) (V4 1 1 1 1)
    ]

drawTexture :: Renderer -> Text -> V2 Float -> V2 Float -> IO ()
drawTexture renderer name
  (V2 x y)
  (V2 w h) = do
  (texture, textureRect) <- getTexture renderer name

  let
    (V4
      (fromIntegral -> tx) (fromIntegral -> ty)
      (fromIntegral -> tw) (fromIntegral -> th)) = textureRect

  let vertices = makeQuad (V4 x y w h) (V4 tx ty tw th)

  let
    vertexSize = sizeOf @Vertex undefined
    verticesSize = fromIntegral $ vertexSize * Vector.length vertices

  Vector.unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (verticesSize, ptr, GL.DynamicDraw)

  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture

  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GL.textureBinding GL.Texture2D $= Nothing

drawSprite :: Renderer -> Text -> Int -> V2 Float -> V2 Float -> IO ()
drawSprite renderer name cell
  (V2 x y)
  (V2 w h) = do
  (texture, textureRect) <- getTexture renderer name
  spritesheet <- getSpritesheet renderer name

  let
    size = spritesheet.spritesheetCellSize
    maybePos = spritesheet.spritesheetCells !? cell
  pos <- case maybePos of
    Just x -> pure x
    Nothing -> do
      hPutStrLn stderr "wha??????? no sprite???????????"
      exitFailure

  let
    textureRect' = (over _xy (+ pos) . over _zw (const size)) textureRect
    (V4
      (fromIntegral -> tx) (fromIntegral -> ty)
      (fromIntegral -> tw) (fromIntegral -> th)) = textureRect'
  
  let vertices = makeQuad (V4 x y w h) (V4 tx ty tw th)

  let
    vertexSize = sizeOf @Vertex undefined
    verticesSize = fromIntegral $ vertexSize * Vector.length vertices

  Vector.unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (verticesSize, ptr, GL.DynamicDraw)

  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just texture

  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GL.textureBinding GL.Texture2D $= Nothing

updateViewport :: Renderer -> Int32 -> Int32 -> IO ()
updateViewport Renderer{ rendererShader = maybeShader }
  w@(fromIntegral -> w') h@(fromIntegral -> h') = do
  GL.viewport $= (GL.Position 0 0, GL.Size w h)
  case maybeShader of
    Just shader -> setUniform shader "u_projection" =<< m44ToGL (ortho 0 w' h' 0 (-1) 1)
    Nothing -> pure ()

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
