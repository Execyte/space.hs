module Client.Components.Visual ( Renderable(..)
  , SpriteType(..)
  , VisualAsset(..)
  ) where

import System.IO

import Apecs
import qualified Data.Text as T
import qualified SDL

import Linear (V4 (..), V3 (..), V2 (..))

data SpriteType a =
    SingleSprite a Int Int -- path, size
  | AnimationSprite a Int Int Int -- path, size, speed
  deriving Show

data VisualAsset a =
    DirectionalSpritesMap { up :: a, down :: a, left :: a, right :: a } -- usually only for entities
  | Sprite a -- everything (machines, walls etc)
  | HybridSprite (VisualAsset a, VisualAsset a) -- on the floor and in hand sprites (SingleSprite, DirectionalSpritesMap)
  deriving Show

-- VisualAsset SpriteType FilePath is so real

type Asset a = (VisualAsset (SpriteType a), Int) -- paths, layer

data Renderable = Renderable { sprite :: Asset FilePath
  , position :: V2 Double -- this is just an offset from the Position component in x and y only usually.
  , orientation :: V2 Double
  , color :: V4 Double
  , parent :: Entity -- if the parent is global, it's position and orientation are inherited from the entity itself, while if the parent is a player (for example: items) it'll inherit it's position and orienation from the player's components. This is used for clothes and items.
  }
  deriving Show

instance Component Renderable where type Storage Renderable = Map Renderable
