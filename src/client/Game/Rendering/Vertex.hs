module Game.Rendering.Vertex (
  VertexAttrib(..),
  VertexAttribs(..),
  Vertex(..),
  setVertexAttribs
) where

import Data.Foldable
import Foreign

import Linear
import Data.StateVar
import Graphics.Rendering.OpenGL qualified as GL

glSizeOf :: GL.DataType -> Int
glSizeOf t = case t of
  GL.UnsignedByte -> sizeOf @Word8 undefined
  GL.Byte -> sizeOf @Int8 undefined
  GL.UnsignedShort -> sizeOf @Word16 undefined
  GL.Short -> sizeOf @Int16 undefined
  GL.UnsignedInt -> sizeOf @Word32 undefined
  GL.Int -> sizeOf @Int32 undefined
  GL.Float -> sizeOf @Float undefined
  GL.Double -> sizeOf @Double undefined
  _ -> error "unhandled vertex attribute type"

data VertexAttrib = VertexAttrib Int Int GL.DataType

class Storable a => VertexAttribs a where
  vertexAttribs :: a -> [VertexAttrib]

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

instance VertexAttribs Vertex where
  vertexAttribs _ = [
      VertexAttrib 0 2 GL.Float
    , VertexAttrib 1 2 GL.Float
    , VertexAttrib 2 4 GL.Float
    ]

setVertexAttribs :: forall a. VertexAttribs a => a -> GL.VertexArrayObject -> IO ()
setVertexAttribs _ vertexArray = do
  vertexArray' <- get GL.bindVertexArrayObject
  GL.bindVertexArrayObject $= Just vertexArray

  let attribs = vertexAttribs @a undefined

  foldlM setVertexAttrib nullPtr attribs

  GL.bindVertexArrayObject $= vertexArray'
  where
    setVertexAttrib offset (VertexAttrib location count type') = do
      let
        stride = fromIntegral $ sizeOf @a undefined
        typeSize = glSizeOf type'
        attribSize = count * typeSize
        location' = fromIntegral location
        count' = fromIntegral count
      
      GL.vertexAttribPointer (GL.AttribLocation location') $=
        (GL.ToFloat, GL.VertexArrayDescriptor count' type' stride offset)
      GL.vertexAttribArray (GL.AttribLocation location') $= GL.Enabled

      pure $ offset `plusPtr` attribSize
