module Shared.Components.Physics(Transform(..), LinearVelocity(..), Collidable(..)) where

import Linear
import Apecs

import Data.Int (Int64)

-- | V3 because z layers.
-- Int64 - tile based supremacy.
newtype Transform = Transform (V3 Int64) deriving Show
instance Component Transform where type Storage Transform = Map Transform

newtype LinearVelocity = LinearVelocity (V3 Int64) deriving Show
instance Component LinearVelocity where type Storage LinearVelocity = Map LinearVelocity

-- | allows an object to collide into other objects
data Collidable = Collidable deriving Show
instance Component Collidable where type Storage Collidable = Map Collidable
