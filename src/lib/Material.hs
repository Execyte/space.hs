module Material(Material(..), iron, glass) where

import Shared.Components.Damage

import Data.Text (Text)

-- | defines a type of material
data Material =
  Material
    Text -- ^ name of the material
    [DamageModifier] -- ^ resistances of the material
    Float -- ^ no idea, figure out later
  deriving (Eq, Show)

iron :: Material
iron = Material "Iron"
  [ DamageModifierNum Blunt 12
  , DamageModifierNum Slash 22
  , DamageModifierNum Pierce 16
  ] 1.25

glass :: Material
glass = Material "Glass"
  [ DamageModifierPrecentage Blunt 140
  , DamageModifierPrecentage Slash 140
  , DamageModifierPrecentage Pierce 140
  ] 1.05
