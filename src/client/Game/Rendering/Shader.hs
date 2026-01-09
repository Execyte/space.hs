module Game.Rendering.Shader (
  Shader,
  shaderFromByteStrings,
  shaderFromFiles,
  setUniform
) where

import Data.List
import Data.Maybe
import Control.Monad ((<=<))
import Data.Traversable
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

shaderFromByteStrings :: [(GL.ShaderType, ByteString)] -> IO (Maybe Shader, String)
shaderFromByteStrings shaders = do
  let
    compile program (type', code) = do
      (maybeShader, logs) <- createShader type' code

      error <- case maybeShader of
        Just shader -> do
          GL.attachShader program shader
          GL.deleteObjectName shader
          pure Nothing
        Nothing -> pure $ Just logs
      
      pure (program, error)

  (program, results) <- flip (mapAccumM compile) shaders =<< GL.createProgram

  case catMaybes results of
    [] -> do
      GL.linkProgram program
      GL.validateProgram program

      linked <- GL.linkStatus program
      valid <- GL.validateStatus program
      logs <- GL.programInfoLog program

      pure (if linked && valid then Just program else Nothing, logs)
    errors -> pure (Nothing, intercalate "\n" errors)

shaderFromFiles :: [(GL.ShaderType, FilePath)] -> IO (Maybe Shader, String)
shaderFromFiles = shaderFromByteStrings <=< traverse readShader
  where readShader (type', path) = (type',) <$> ByteString.readFile path

setUniform :: GL.Uniform a => Shader -> String -> a -> IO ()
setUniform shader name value = do
  GL.currentProgram $= Just shader
  uniform <- GL.uniformLocation shader name
  GL.uniform uniform $= value
  GL.currentProgram $= Nothing

