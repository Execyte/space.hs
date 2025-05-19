module Client.Components.Visual
  ( Renderable(..)
  , SpriteData(..)
  , Color(..)
  , Sprite(..)
  , defaultSpriteData64
  , defaultSpriteData32
  , defaultSpriteData16
  , defaultSpriteData8
  ) where

import System.IO

import Apecs
import qualified Data.Text as T
import qualified SDL

import Linear (V4 (..), V3 (..), V2 (..))

-- note from Execute: don't overcomplicate these things...
-- I made it so it can take in multiple types of sprites (wearing, in hand, on the floor) and that was a MISTAKE
-- in ECS, u render things based on their state, not this one single component
--
-- use the Transform component for position and orientation, use the Color component for color
-- Renderable denotes EVERY SINGLE SPRITE. In hand, while wearing, etc
-- it points to a folder of sprites in which the sprite is picked either main.png, inhand.png or wearing.png

-- | speed (for animations) and sprite size
data SpriteData = SpriteData Int Int
  deriving Show

data Sprite a = Sprite SpriteData a
  deriving Show

type TextureList = Sprite FilePath

-- | position and orientation for smooth interpolation, don't use otherwise
data Renderable = Renderable
  { _textureList :: TextureList
  , _position :: (V3 Double)
  , _orientation :: Double 
  , _drawDepth :: Int
  }
  deriving Show

newtype Color = Color (V3 Float)
  deriving Show

instance Component Renderable where type Storage Renderable = Map Renderable
instance Component Color where type Storage Color = Map Color

defaultSpriteData64 :: SpriteData
defaultSpriteData64 = SpriteData 0 64

defaultSpriteData32 :: SpriteData
defaultSpriteData32 = SpriteData 0 32

defaultSpriteData16 :: SpriteData
defaultSpriteData16 = SpriteData 0 16

defaultSpriteData8 :: SpriteData
defaultSpriteData8 = SpriteData 0 8
