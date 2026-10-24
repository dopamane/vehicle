{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Vehicle (Vehicle(..), Regulatory(..), Mission(..), Category(..)
  , Class(..), LaunchOrientation(..), LaunchDomain(..), LandingOrientation(..)
  , Recovery(..), Mode(..), Capability(..), TermSys(..), Size(..)
  , Propellant(..), Payload(..)) where

import Data.Binary
import GHC.Generics
import Prettyprinter

data Vehicle = Vehicle{reg :: Regulatory, missn :: Mission, cat :: Category
  , clas :: Class, mode :: Mode, caps :: [Capability], siz :: Size
  , props :: [Propellant], payload :: Payload}
  deriving (Binary, Eq, Generic, Read, Show)

data Regulatory = Gov | Com deriving (Binary, Eq, Generic, Read, Show)

instance Pretty Regulatory where
  pretty Gov = "Government"
  pretty Com = "Commercial"

data Mission = Gov4Gov | Com4Gov | Com4Com | Gov4Com
  deriving (Binary, Eq, Generic, Read, Show)

data Category = Rocket | Spaceplane | LiftingBody | Capsule | Balloon
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty Category where
  pretty = viaShow

data Class = Class{launchOrien :: LaunchOrientation, launchDom :: LaunchDomain
  , landingOrien :: LandingOrientation, recovery :: Recovery}
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty Class where
  pretty c = vsep ["Launch Orientation:" <+> pretty (launchOrien c)
    , "Launch Domain:" <+> pretty (launchDom c)
    , "Landing Orientation:" <+> pretty (landingOrien c)
    , "Recovery:" <+> pretty (recovery c)]

data LaunchOrientation = VertLaunch | HorizLaunch | PayldLaunch
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty LaunchOrientation where
  pretty VertLaunch = "Vertical"
  pretty HorizLaunch = "Horizontal"
  pretty PayldLaunch = "Payload"

data LaunchDomain = AirLaunch | GroundLaunch | SeaLaunch | SpaceLaunch
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty LaunchDomain where
  pretty l = case l of
    AirLaunch -> "Air"
    GroundLaunch -> "Ground"
    SeaLaunch -> "Sea"
    SpaceLaunch -> "Space"

data LandingOrientation = VertLand | HorizLand
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty LandingOrientation where
  pretty VertLand = "Vertical"
  pretty HorizLand = "Horizontal"

data Recovery = AirRecovery | GroundRecovery | SeaRecovery
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty Recovery where
  pretty AirRecovery = "Air"
  pretty GroundRecovery = "Ground"
  pretty SeaRecovery = "Sea"

data Mode = Piloted | UnguidedBallistic | AutoGuided | GuidedExtCmds
  | AutoPilotHybrid deriving (Binary, Eq, Generic, Read, Show)

instance Pretty Mode where
  pretty m = case m of
    Piloted -> "Piloted"
    UnguidedBallistic -> "Unguided (Ballistic)"
    AutoGuided -> "Autonomously Guided"
    GuidedExtCmds -> "Guided with External Commands"
    AutoPilotHybrid -> "Autonomous and Piloted Hybrid"

data Capability = AntiCollision | Propulsive | StationaryPwrdFlight String |
  FastSlowApproachLandingSpeed [(Double, Double)] Double | GearCfg String | JettiBody String |
  Stage Integer Bool (Maybe Bool) | TermSys TermSys
  deriving (Binary, Eq, Generic, Read, Show)

data TermSys = Thrust | OrdFTS | FlightCtlManip | Other
  deriving (Binary, Eq, Generic, Read, Show)

data Size = Size{takeoffWeights :: [(Integer, (Double, Double))]
  , landingWeights :: [(Integer, (Double, Double))], explosiveEquiv :: (Double, Double)
  , thrustProf :: [(Double, Double)]} deriving (Binary, Eq, Generic, Read, Show)

data Propellant = Propellant{propSysType :: String, fuelCap :: Double
  , oxidCap :: Double, otherProps :: [(String, Double)]}
  deriving (Binary, Eq, Generic, Read, Show)

data Payload = Humans | LiveAnimals | LiveViableBiologics
  | NonLivingNonHazardCargo | HazardCargo
  deriving (Binary, Eq, Generic, Read, Show)
