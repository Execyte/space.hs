module Components(ServerEntity, ClientEntity, Camera(..), Client(..)) where

import Apecs
import Apecs.TH (makeWorld, makeMapComponents)

import Data.Semigroup
import Data.Monoid

import Linear

type ServerEntity = Entity
type ClientEntity = Entity

newtype Camera = Camera (V2 Float) deriving Show
instance Component Camera where
  type Storage Camera = Global Camera
instance Semigroup Camera where (Camera p1) <> (Camera p2) = Camera $ (p1 ^+^ p2)
instance Monoid Camera where mempty = Camera $ V2 0 0

data Client = Client deriving Show
instance Component Client where
  type Storage Client = Unique Client
