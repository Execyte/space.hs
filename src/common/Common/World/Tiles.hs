module Common.World.Tiles(Tiles(..), Layers(..), Tile(..), getTopmostLayer) where

import Control.Applicative

data Tiles a = Tiles {floor, wall :: a}
  deriving (Functor, Foldable, Traversable)

data Layers a = Layers {center, floor :: Maybe a}
  deriving (Functor, Foldable, Traversable)

data Tile = Tile {health :: Int}

getTopmostLayer :: Layers a -> Maybe a
getTopmostLayer (Layers{center, floor}) = center <|> floor
