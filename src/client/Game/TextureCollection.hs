module Game.TextureCollection(TextureCollection(..)) where

data TextureCollection a = TextureCollection
  { floor :: a
  , wall :: a
  , humanFacingUp, humanFacingDown, humanFacingLeft, humanFacingRight :: a
  }
