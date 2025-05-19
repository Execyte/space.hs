module Shared.Components.Movement() where

import Apecs

data MovementState = 
    Running
  | Walking
  | Crouching
  | Proning
  deriving (Eq, Show)

data Movable =
  Movable
    MovementState -- ^ state of moving
    Float -- ^ movement speed
  deriving Show
instance Component Movable where type Storage Movable = Map Movable
