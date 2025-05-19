module Shared.Components.Tiles
  ( TileLayer(..)
  , lattice
  , plating
  ) where

import Data.Text (Text)
import Shared.Components.Damage

data TileLayer =
  TileLayer
    Text -- ^ name of the layer
    Float -- ^ integrity of the layer
    [TileLayer] -- ^ whitelisted layers in which this layer can be placed on
    [DamageModifier] -- ^ resistances of this later
  deriving (Eq, Show)

lattice :: TileLayer
lattice = TileLayer "Lattice" 50 [] []

plating :: TileLayer
plating = TileLayer "Plating" 50 [lattice] []
