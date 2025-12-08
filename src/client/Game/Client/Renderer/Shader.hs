module Game.Client.Renderer.Shader (
  Shader,
  fromByteStrings,
  fromFiles,
  setUniform
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.StateVar

import Graphics.Rendering.OpenGL qualified as GL

type Shader = GL.Program

createShader :: GL.ShaderType -> ByteString -> IO (Maybe GL.Shader, String)
createShader type' code = do
  shader <- GL.createShader type'
  GL.shaderSourceBS shader $= code
  GL.compileShader shader

  success <- GL.compileStatus shader
  logs <- GL.shaderInfoLog shader

  return (if success then Just shader else Nothing, logs)

fromByteStrings :: [(GL.ShaderType, ByteString)] -> IO (Maybe Shader, String)
fromByteStrings shaders = attach shaders =<< GL.createProgram
  where
    attach ((type', code):shaders) program = do
      (maybeShader, logs) <- createShader type' code

      case maybeShader of
        Just shader -> do
          GL.attachShader program shader
          GL.deleteObjectName shader
          attach shaders program
        Nothing -> do
          return (Nothing, logs)
    attach [] program = do
      GL.linkProgram program
      GL.validateProgram program

      linked <- GL.linkStatus program
      valid <- GL.validateStatus program
      logs <- GL.programInfoLog program

      return (if linked && valid then Just program else Nothing, logs)

fromFiles :: [(GL.ShaderType, FilePath)] -> IO (Maybe Shader, String)
fromFiles shaders = attach shaders =<< GL.createProgram
  where
    attach ((type', file):shaders) program = do
      code <- ByteString.readFile file

      (maybeShader, logs) <- createShader type' code

      case maybeShader of
        Just shader -> do
          GL.attachShader program shader
          GL.deleteObjectName shader
          attach shaders program
        Nothing -> do
          return (Nothing, logs)
    attach [] program = do
      GL.linkProgram program
      GL.validateProgram program

      linked <- GL.linkStatus program
      valid <- GL.validateStatus program
      logs <- GL.programInfoLog program

      return (if linked && valid then Just program else Nothing, logs)

setUniform :: GL.Uniform a => Shader -> String -> a -> IO ()
setUniform shader name value = do
  GL.currentProgram $= Just shader
  uniform <- GL.uniformLocation shader name
  GL.uniform uniform $= value
  GL.currentProgram $= Nothing
