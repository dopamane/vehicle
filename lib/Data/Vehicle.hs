{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Vehicle (Vehicle(..), Reg(..), MissnTy(..), Cat(..), Class(..)
  , LnchOrie(..), LnchDom(..), LndOrie(..), Recov(..), Mode(..), Cap(..)
  , TSys(..), Size(..), Prop(..), Payld(..)
  ) where

import Data.Binary
import GHC.Generics

data Vehicle = Vehicle{reg :: Reg, missnTy :: MissnTy, cat :: Cat
  , clas :: [Class], mode :: Mode, caps :: [Cap], sizs :: [Size], props :: [Prop]
  , payld :: [Payld]}
  deriving (Binary, Eq, Generic, Read, Show)

data Reg = Gov String | Com String deriving (Binary, Eq, Generic, Read, Show)

data MissnTy = GovGov String | ComGov String | ComCom String | GovCom String
  deriving (Binary, Eq, Generic, Read, Show)

data Cat = Rocket | Spaceplane | LiftingBody | Capsule | Balloon
  deriving (Binary, Eq, Generic, Read, Show)

data Class = LnchOrie LnchOrie | LnchDom LnchDom | LndOrie LndOrie | Recov Recov
  deriving (Binary, Eq, Generic, Read, Show)

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

data Cap = AntiColisn | Proplsv | StatPwrFlight Double |
  FastSlowApprchLndingSpd [(Double, Double)] Double | GearCfg String | JetiBdy String |
  Stge Integer Bool (Maybe Bool) | TermSys TSys
  deriving (Binary, Eq, Generic, Read, Show)

data TSys = Thrustt | OrdFTS | FlightCtlManip | Ots String
  deriving (Binary, Eq, Generic, Read, Show)

data Size = LnchSz Integer Double Double | RntrySz Integer Double Double
  | Explosv Double Double | Thrustp [(Double, Double)]
  deriving (Binary, Eq, Generic, Read, Show)

data Prop = Sys String | Fuel String Double | Oxidizr String Double | OtherProp String Double
  deriving (Binary, Eq, Generic, Read, Show)

data Payld = Human String | Animal String | Biologic String | Cargo String
  | Hazard String
  deriving (Binary, Eq, Generic, Read, Show)
