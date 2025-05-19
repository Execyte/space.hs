-- | Contains all the parts of the damage system
module Shared.Components.Damage( Damage(..), DamageType(..), DamageModifier(..) ) where

import Apecs

-- | type of damage of the following:
--
-- Slash, Blunt and Pierce - Brute
--
-- Radiation, Causitc, Heat, Cold, Shock - Burn
--
-- Poison - Toxin
--
-- Integral - Structural
--
-- Bloodloss, Asphyxiation - Airloss
--
-- Nerve, Stamina, Genetic, Bleeding - General
data DamageType =
    Slash | Blunt | Pierce -- Brute
  | Radiation | Caustic | Heat | Cold | Shock -- Burn
  | Poison -- Toxin
  | Integral -- Structural
  | Bloodloss | Asphyxiation -- Airloss
  | Nerve | Stamina | Genetic | Bleeding -- General
  deriving (Eq, Show)

-- | usually used for damage resistances, denotes a modification to damage
data DamageModifier =
    DamageModifierPrecentage -- ^ modify the damage by a precentage
      DamageType -- ^ type of damage to modify
      Int -- ^ precentage from 0 - 100
  | DamageModifierNum -- ^ modify the damage by a number
      DamageType -- ^ type of damage to modify
      Double -- ^ a number
  deriving (Eq, Show)

newtype Damage = Damage [(DamageType, Double, Entity)] deriving Show
instance Component Damage where type Storage Damage = Map Damage
