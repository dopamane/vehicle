module Data.Vehicle (Vehicle(..)) where

data Vehicle = Vehicle{reg :: Regulatory, missn :: Mission, cat :: Category
  , clas :: Class, mode :: Mode, cap :: Capability, siz :: Size
  , prop :: Propellant}
  deriving (Eq, Read, Show)

data Regulatory = Gov | Com deriving (Eq, Read, Show)

data Mission = Gov4Gov | Com4Gov | Com4Com | Gov4Com deriving (Eq, Read, Show)

data Category = Rocket | Spaceplane | LiftingBody | Capsule | Balloon
  deriving (Eq, Read, Show)

data Class = Class deriving (Eq, Read, Show)

data Mode = Mode deriving (Eq, Read, Show)

data Capability = Capability deriving (Eq, Read, Show)

data Size = Size deriving (Eq, Read, Show)

data Propellant = Propellant deriving (Eq, Read, Show)
