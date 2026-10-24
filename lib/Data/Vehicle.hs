{-# LANGUAGE OverloadedStrings #-}

module Data.Vehicle (Vehicle(..), Regulatory(..)) where

import Prettyprinter

data Vehicle = Vehicle{reg :: Regulatory, missn :: Mission, cat :: Category
  , clas :: Class, mode :: Mode, cap :: Capability, siz :: Size
  , prop :: Propellant} deriving (Eq, Read, Show)

data Regulatory = Gov | Com deriving (Eq, Read, Show)

instance Pretty Regulatory where
  pretty Gov = "Government"
  pretty Com = "Commercial"

data Mission = Gov4Gov | Com4Gov | Com4Com | Gov4Com deriving (Eq, Read, Show)

data Category = Rocket | Spaceplane | LiftingBody | Capsule | Balloon
  deriving (Eq, Read, Show)

data Class = Class{launchOrien :: LaunchOrientation, launchDom :: LaunchDomain
  , landingOrien :: LandingOrientation, recovery :: Recovery}
  deriving (Eq, Read, Show)

data LaunchOrientation = VertLaunch | HorizLaunch | PayldLaunch
  deriving (Eq, Read, Show)

data LaunchDomain = AirLaunch | GroundLaunch | SeaLaunch | SpaceLaunch
  deriving (Eq, Read, Show)

data LandingOrientation = VertLand | HorizLand deriving (Eq, Read, Show)

data Recovery = AirRecovery | GroundRecovery | SeaRecovery
  deriving (Eq, Read, Show)

data Mode = Mode deriving (Eq, Read, Show)

data Capability = Capability deriving (Eq, Read, Show)

data Size = Size deriving (Eq, Read, Show)

data Propellant = Propellant deriving (Eq, Read, Show)
