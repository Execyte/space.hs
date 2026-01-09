module Game.Textures(Texture(..), Textures(..)) where

data Texture = Texture

data Textures a = Textures
  { floor :: a
  , wall :: a
  , humanFacingUp, humanFacingDown, humanFacingLeft, humanFacingRight :: a
  }
