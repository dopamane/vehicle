{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Vehicle (Vehicle(..), Reg(..), MissnTy(..), Cat(..), Class(..)
  , LnchOri(..), LnchDom(..), LndOri(..), Recov(..), Mode(..), Cap(..)
  , TSys(..), Size(..), Prop(..), Payld(..), Missn(..), Plot
  ) where

import Data.Binary
import GHC.Generics

data Vehicle = Vehicle{reg :: Reg, missnTy :: MissnTy, cat :: Cat
  , clas :: [Class], mode :: Mode, caps :: [Cap], sizs :: [Size], props :: [Prop]
  , payld :: [Payld], missn :: [Missn]}
  deriving (Binary, Eq, Generic, Read, Show)

data Reg = Gov String | Com String deriving (Binary, Eq, Generic, Read, Show)

data MissnTy = GovGov String | ComGov String | ComCom String | GovCom String
  deriving (Binary, Eq, Generic, Read, Show)

data Cat = Rocket | Spaceplane | LiftingBody | Capsule | Balloon
  deriving (Binary, Bounded, Enum, Eq, Generic, Read, Show)

data Class = LnchOri LnchOri | LnchDom LnchDom | LndOri LndOri | Recov Recov
  deriving (Binary, Eq, Generic, Read, Show)

-- | Launch Orientation
data LnchOri = VertLnch | HorizLnch | PayldLnch
  deriving (Binary, Bounded, Enum, Eq, Generic, Read, Show)

-- | Launch domain
data LnchDom = Air | Gnd | Sea | Spce
  deriving (Binary, Bounded, Enum, Eq, Generic, Read, Show)

-- | Landing Orientation
data LndOri = VertLand | HorizLand
  deriving (Binary, Bounded, Enum, Eq, Generic, Read, Show)

-- | Recovery
data Recov = AirRec | GndRec | SeaRec
  deriving (Binary, Bounded, Enum, Eq, Generic, Read, Show)

data Mode = Piloted | UnguidedBallistic | AutoGuided | GuidedExtCmds
  | AutoPilotHybrid deriving (Binary, Bounded, Enum, Eq, Generic, Read, Show)

data Cap = AntiColisn | Proplsv | StatPwrFlight Double |
  FastSlowApprchLndingSpd String Plot Double | GearCfg String | JetiBdy String |
  Stge Int Bool (Maybe Bool) | TermSys TSys
  deriving (Binary, Eq, Generic, Read, Show)

data TSys = Thrustt | OrdFTS | FlightCtlManip | Ots String
  deriving (Binary, Eq, Generic, Read, Show)

data Size = LnchSz Int Double Double | RntrySz Int Double Double
  | Explosv Double Double | Thrustp String Plot
  deriving (Binary, Eq, Generic, Read, Show)

data Prop = Sys String | Fuel String Double | Oxidizr String Double | OtherProp String Double
  deriving (Binary, Eq, Generic, Read, Show)

data Payld = Human String | Animal String | Biologic String | Cargo String
  | Hazard String
  deriving (Binary, Eq, Generic, Read, Show)

data Missn = Orb String Plot | Suborb String Plot
  | NASUsage String Int Int Plot | Windw String Int Int
  | Flex String Int | AdvNoticeOp String Double
  deriving (Binary, Eq, Generic, Read, Show)

type Plot = [(Double, Double)]
