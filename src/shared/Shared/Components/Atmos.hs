module Shared.Components.Atmos( Mixture(..)
  , ReactionDef(..)
  , Gas(..)
  , AtmosContainer(..)
  , Temperature(..)
  , oxygen, nitrogen, hydrogen, waterVapor
  , hydrogenFire, waterVaporCondensation
  ) where

import Apecs
import Data.Text (Text, pack, unpack)

type Mol = Float

data GasDef = GasDef Text Text Float deriving Eq

instance Show GasDef where
  show (GasDef x y _) = unpack $ x <> (pack " (") <> y <> (pack ")")

-- | gas factors required for a gas reaction to be present.
type Factor = GasDef

data ReactionDef = ReactionDef Text [Factor] deriving (Eq, Show)

data Gas = Gas GasDef Mol Double deriving Show

data Mixture = Mixture { volume :: Double -- ^ liters
  , temperature :: Double -- ^ kelvin
  , pressure :: Double -- ^ pascals
  , energy :: Double -- ^ joules
  , heat_capacity :: Float -- ^ total heat capacity
  , gases :: [Gas]
  , reactions :: [ReactionDef]
  } deriving Show

data AtmosContainer =
  AtmosContainer
    Double -- ^ pressure cap
    Mixture
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
