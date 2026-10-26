module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Vehicle
import Data.Version
import Options.Applicative
import Paths_vehicle

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ReadVehicle fM -> maybe BS.getContents BS.readFile fM >>= \bs ->
      print $ runGet (get :: Get Vehicle) bs
    WriteVehicle v fM -> maybe BS.putStr BS.writeFile fM $ runPut $ put v

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Space Launch & Reentry Vehicles"

data CLI = ReadVehicle (Maybe FilePath) | WriteVehicle Vehicle (Maybe FilePath)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [command "r" $ info (ReadVehicle <$> optional fileArg) $ progDesc "Read vehicle data"
  , command "w" $ info (WriteVehicle <$> parseVehicle <*> optional fileArg) $
    progDesc "Write vehicle data"]

fileArg :: Parser FilePath
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN"

parseVehicle :: Parser Vehicle
parseVehicle = Vehicle <$> parseReg <*> parseMissn <*> parseCat <*> parseClass
  <*> parseMode <*> many parseCap <*> parseSiz <*> many parseProp <*> parsePayload

parseReg :: Parser Regulatory
parseReg = flag' Gov (long "gov" <> help "Government regulatory") <|>
  flag' Com (long "com" <> help "Commerical regulatory")

parseMissn :: Parser Mission
parseMissn = asum
  [ flag Gov4Gov Gov4Gov $ long "gov-gov" <> help "Government for Government default"
  , flag' Com4Gov $ long "com-gov" <> help "Commerical for Government"
  , flag' Com4Com $ long "com-com" <> help "Commerical for Commercial"
  , flag' Gov4Com $ long "gov-com" <> help "Government for Commerical"
  ]

parseCat :: Parser Category
parseCat = asum
  [ flag Rocket Rocket $ long "rocket", flag' Spaceplane $ long "spaceplane",
    flag' LiftingBody $ long "lifting-body", flag' Capsule $ long "capsule",
    flag' Balloon $ long "balloon"]

parseClass :: Parser Class
parseClass = Class <$> parseLaunchOrien <*> parseLaunchDom <*>
  parseLandingOrien <*> parseRecovery

parseLaunchOrien :: Parser LaunchOrientation
parseLaunchOrien = asum [flag VertLaunch VertLaunch $ long "vert"
  , flag' HorizLaunch $ long "horiz", flag' PayldLaunch $ long "payld"]

parseLaunchDom :: Parser LaunchDomain
parseLaunchDom = asum [flag AirLaunch AirLaunch $ long "air",
  flag' GroundLaunch $ long "ground", flag' SeaLaunch $ long "sea",
  flag' SpaceLaunch $ long "space"]

parseLandingOrien :: Parser LandingOrientation
parseLandingOrien = flag VertLand VertLand (long "vert") <|>
  flag' HorizLand (long "horiz")

parseRecovery :: Parser Recovery
parseRecovery = flag AirRecovery AirRecovery (long "air") <|>
  flag' GroundRecovery (long "ground") <|> flag' SeaRecovery (long "sea")

parseMode :: Parser Mode
parseMode = asum [flag Piloted Piloted $ long "piloted",
  flag' UnguidedBallistic $ long "unguided-ballistic",
  flag' AutoGuided $ long "auto-guided", flag' GuidedExtCmds $ long "guided-ext"
  , flag' AutoPilotHybrid $ long "auto-pilot-hybrid"]

parseCap :: Parser Capability
parseCap = asum
  [ flag' AntiCollision $ long "anti-collision"
  , flag' Propulsive $ long "propulsive"
  , fmap StationaryPwrdFlight $ strOption $ long "stat-pwr-flight"
    <> help "Stationary powered flight"
  , FastSlowApproachLandingSpeed <$> many parsePt <*>
    option auto (long "landing-speed")
  , GearCfg <$> strOption (long "gear-cfg")
  , JettiBody <$> strOption (long "jetti-bdy")
  , Stage <$> option auto (long "stage") <*> switch (long "ctl-reentry") <*>
    optional (switch $ long "reusable")
  , TermSys <$> parseTermSys
  ]

parseTermSys :: Parser TermSys
parseTermSys = asum
  [ flag Thrust Thrust $ long "thrust"
  , flag' OrdFTS $ long "ord-fts" <>
    help "Ordnance flight termination system (FTS)"
  , flag' FlightCtlManip $ long "flight-ctl-manip" <>
    help "Flight control manipulation"
  , flag' Other $ long "other"]

parsePt :: Parser (Double, Double)
parsePt = (,) <$> option auto (short 'x') <*> option auto (short 'y')

parseSiz :: Parser Size
parseSiz = undefined

parseProp :: Parser Propellant
parseProp = Propellant <$> strOption (long "system-type") <*>
  option auto (long "fuel-cap") <*> option auto (long "oxidize") <*>
  many ((,) <$> strOption (long "other-ident") <*> option auto (long "other-cap"))

parsePayload :: Parser Payload
parsePayload = asum [flag Humans Humans $ long "humans",
  flag' LiveAnimals $ long "live-animals",
  flag' LiveViableBiologics $ long "live-viable-bio",
  flag' NonLivingNonHazardCargo $ long "non-living-non-hazard",
  flag' HazardCargo $ long "hazard"]
