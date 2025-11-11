module Client.Renderer (
  Renderer(..)
) where

import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import Client.Renderer.Shader (Shader)

data Renderer = Renderer {
  window :: SDL.Window,
  shader :: Shader,
  texture :: GL.TextureObject
}