-- | Contains all the parts of the atmos system
module Shared.Components.Atmos( Mixture(..)
  , ReactionDef(..)
  , GasDef(..)
  , Gas(..)
  , AtmosContainer(..)
  , Temperature(..)
  , oxygen, nitrogen, hydrogen, waterVapor
  , hydrogenFire, waterVaporCondensation
  ) where

import Apecs
import Data.Text (Text, pack, unpack)

-- | definition of a gas
data GasDef = GasDef
  Text -- ^ gas name (Hydrogen)
  Text -- ^ molecular gas name (H₂)
  Float -- ^ specific heat capacity
  deriving Eq

instance Show GasDef where
  show (GasDef x y _) = unpack $ x <> (pack " (") <> y <> (pack ")")

-- | definition of a reaction type
data ReactionDef =
  ReactionDef
    Text -- ^ name of the reaction
    [GasDef] -- ^ gas factors of this reaction
  deriving (Eq, Show)

-- | definition of a gas instance
data Gas = Gas
  GasDef -- ^ type of gas
  Double -- ^ amount of mols
  Double -- ^ 
  deriving Show

-- | a mixture that contains gases and calculates reactions
data Mixture = Mixture { volume :: Double -- ^ liters
  , temperature :: Double -- ^ kelvin
  , pressure :: Double -- ^ pascals
  , energy :: Double -- ^ joules
  , heat_capacity :: Float -- ^ total heat capacity
  , gases :: [Gas] -- ^ list of gases in the mixture
  , reactions :: [ReactionDef] -- ^ list of reactions present in the mixture
  } deriving Show

data AtmosContainer =
  AtmosContainer
    Double -- ^ pressure cap
    Mixture -- ^ mixture of this container
  deriving Show

instance Component AtmosContainer where type Storage AtmosContainer = Map AtmosContainer

-- | temperature of entities
newtype Temperature = Temperature Double deriving Show
instance Component Temperature where type Storage Temperature = Map Temperature

data AtmosFlag =
  AtmosVoid -- ^ atmospherics flag to instantly wipe all gases in the mixture.
  deriving Show

newtype AtmosFlags = AtmosFlags { unAtmosFlags :: [AtmosFlag] }
instance Component AtmosFlags where type Storage AtmosFlags = Map AtmosFlags

oxygen :: GasDef
oxygen = GasDef "Oxygen" "O2" 0 -- no idea abt heat capacity

nitrogen :: GasDef
nitrogen = GasDef "Nitrogen" "N2" 0

hydrogen :: GasDef
hydrogen = GasDef "Hydrogen" "H2" 0

waterVapor :: GasDef
waterVapor = GasDef "Water Vapor" "H2O" 0

-- | Burning hydrogen becomes water.
hydrogenFire :: ReactionDef
hydrogenFire = ReactionDef "Hydrogen Fire" [oxygen, hydrogen] 

-- | Water vapor, when below a certain arbitrary temperature at varying pressures, becomes normal water.
waterVaporCondensation :: ReactionDef
waterVaporCondensation = ReactionDef "Water Vapor Condensation" [waterVapor] 
