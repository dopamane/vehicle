{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Vehicle (Vehicle(..), RegTy(..), MissnTy(..), Cat(..), Class(..)
  , LnchOrie(..), LnchDom(..), LndOrie(..), Recov(..)
  , Mode(..), Capability(..), TermSys(..), Size(..), Prop(..), Payld(..)
  ) where

import Data.Binary
import GHC.Generics

data Vehicle = Vehicle{regTy :: RegTy, missnTy :: MissnTy, cat :: Cat
  , clas :: Class, mode :: Mode, caps :: [Capability], siz :: Size
  , props :: [Prop], payld :: [Payld]}
  deriving (Binary, Eq, Generic, Read, Show)

data RegTy = Gov | Com deriving (Binary, Eq, Generic, Read, Show)

data MissnTy = GovGov | ComGov | ComCom | GovCom
  deriving (Binary, Eq, Generic, Read, Show)

data Cat = Rocket | Spaceplane | LiftingBody | Capsule | Balloon
  deriving (Binary, Eq, Generic, Read, Show)

data Class = Class{lnchOrie :: LnchOrie, lnchDom :: LnchDom, lndOrie :: LndOrie
  , recov :: Recov} deriving (Binary, Eq, Generic, Read, Show)

-- | Launch Orientation
data LnchOrie = VertLnch | HorizLnch | PayldLnch
  deriving (Binary, Eq, Generic, Read, Show)

-- | Launch domain
data LnchDom = Air | Gnd | Sea | Spce
  deriving (Binary, Eq, Generic, Read, Show)

-- | Landing Orientation
data LndOrie = VertLand | HorizLand deriving (Binary, Eq, Generic, Read, Show)

-- | Recovery
data Recov = AirRec | GndRec | SeaRec deriving (Binary, Eq, Generic, Read, Show)

data Mode = Piloted | UnguidedBallistic | AutoGuided | GuidedExtCmds
  | AutoPilotHybrid deriving (Binary, Eq, Generic, Read, Show)

data Capability = AntiCollision | Propulsive | StationaryPwrdFlight String |
  FastSlowApproachLandingSpeed [(Double, Double)] Double | GearCfg String | JettiBody String |
  Stage Integer Bool (Maybe Bool) | TermSys TermSys
  deriving (Binary, Eq, Generic, Read, Show)

data TermSys = Thrust | OrdFTS | FlightCtlManip | Other
  deriving (Binary, Eq, Generic, Read, Show)

data Size = Size{takeoffWeights :: [(Integer, (Double, Double))]
  , landingWeights :: [(Integer, (Double, Double))], explosiveEquiv :: (Double, Double)
  , thrustProf :: [(Double, Double)]} deriving (Binary, Eq, Generic, Read, Show)

data Prop = Sys String | Fuel String Double | Oxidizr String Double | OtherProp String Double
  deriving (Binary, Eq, Generic, Read, Show)

data Payld = Human String | Animal String | Biologic String | Cargo String
  | Hazard String
  deriving (Binary, Eq, Generic, Read, Show)
