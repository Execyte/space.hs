module Shared.Components.Atmos( Mixture(..)
  , ReactionDef(..)
  , Gas(..)
  , AtmosContainer(..)
  , Temperature(..)
  , oxygen, nitrogen, hydrogen, waterVapor
  , hydrogenFire, waterVaporCondensation
  ) where

import Apecs

type Mol = Float

type Name = String
type MolecularIdentifier = String
type HeatCapacity = Float
type Temp = Float

data GasDef = GasDef Name MolecularIdentifier HeatCapacity deriving Eq

instance Show GasDef where
  show (GasDef x y _) = x ++ " (" ++ y ++ ")"

type Factor = GasDef -- gas factors required for a gas reaction to be present.

data ReactionDef = ReactionDef Name [Factor] deriving (Eq, Show)

data Gas = Gas GasDef Mol Temp deriving Show

data Mixture = Mixture { volume :: Float -- liters
  , temperature :: Float -- kelvin (total temperature of the mixture)
  , pressure :: Float -- pascals (total pressure of the mixture)
  , energy :: Float -- joules (total energy of the mixture)
  , heat_capacity :: Float -- total heat capacity of the mixture
  , gases :: [Gas]
  , reactions :: [ReactionDef]
  } deriving Show

type PressureCap = Float

data AtmosContainer = AtmosContainer PressureCap Mixture deriving Show
instance Component AtmosContainer where type Storage AtmosContainer = Map AtmosContainer

newtype Temperature = Temperature Float deriving Show
instance Component Temperature where type Storage Temperature = Map Temperature

data AtmosVoid = AtmosVoid deriving Show -- atmospherics flag to instantly wipe all gases in the mixture
instance Component AtmosVoid where type Storage AtmosVoid = Map AtmosVoid
-- we don't ACTUALLY need this for space, as the logic already checks if there are any tile layers available on that tile when trying to transfer to it
-- if there are any tile layers, it will check if gases can pass through that layer, if not, it'll successfully be able to transfer, if yes, it will void the gas.
-- this is just for things such as special kinds of items like vents and such.

-- TODO: add more GAS reactions, actually write the atmos system

oxygen :: GasDef
oxygen = GasDef "Oxygen" "O2" 0 -- no idea abt heat capacity

nitrogen :: GasDef
nitrogen = GasDef "Nitrogen" "N2" 0

hydrogen :: GasDef
hydrogen = GasDef "Hydrogen" "H2" 0

waterVapor :: GasDef
waterVapor = GasDef "Water Vapor" "H2O" 0

hydrogenFire :: ReactionDef
hydrogenFire = ReactionDef "Hydrogen Fire" [oxygen, hydrogen] -- Burning hydrogen becomes water.

waterVaporCondensation :: ReactionDef
waterVaporCondensation = ReactionDef "Water Vapor Condensation" [waterVapor] -- Water vapor, when below a certain arbitrary temperature at varying pressures, becomes normal water.
