module Shared.Components.Tiles
  ( TileLayer(..)
  , lattice
  , plating
  ) where

import Data.Text (Text)
import Shared.Components.Damage

import Material

data TileLayer =
  TileLayer
    Text -- ^ name of the layer
    Float -- ^ integrity of the layer
    [TileLayer] -- ^ whitelisted layers in which this layer can be placed on
    [(Material, Float)] -- ^ materials of this layer, in (Material, Precentage) pairs
  deriving (Eq, Show)

lattice :: TileLayer
lattice = TileLayer "Lattice" 50 [] []

plating :: TileLayer
plating = TileLayer "Plating" 50 [lattice] []
