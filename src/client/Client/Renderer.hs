module Client.Renderer (
  Renderer(..)
) where

import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL

data Renderer = Renderer {
  window :: SDL.Window,
  program :: GL.Program
}