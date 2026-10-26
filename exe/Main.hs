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
    ReadVehicle fM ->
      print . runGet (get :: Get Vehicle) =<< maybe BS.getContents BS.readFile fM
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
parseVehicle = Vehicle <$> parseRegTy <*> parseMissnTy <*> parseCat <*> parseClass
  <*> parseMode <*> many parseCap <*> parseSiz <*> many parseProp <*>
  many parsePayload

parseRegTy :: Parser RegTy
parseRegTy = parserOptionGroup "Regulatory type" $
  flag Gov Gov (long "gov" <> help "Government default") <|>
  flag' Com (long "com" <> help "Commerical")

parseMissnTy :: Parser MissnTy
parseMissnTy = parserOptionGroup "Mission Type" $ asum
  [ flag GovGov GovGov $ long "gov-gov" <> help "Government for Government default"
  , flag' ComGov $ long "com-gov" <> help "Commerical for Government"
  , flag' ComCom $ long "com-com" <> help "Commerical for Commercial"
  , flag' GovCom $ long "gov-com" <> help "Government for Commerical"
  ]

parseCat :: Parser Cat
parseCat = parserOptionGroup "Category" $ asum
  [ flag Rocket Rocket $ long "rocket" <>
    help "propelled and steered by combustion regardless or launch orien or method, default"
  , flag' Spaceplane $ long "spaceplane" <>
    help "winged vehicle, self-propelled, steered by aerodynmic flight ctl in atmosphere, \
         \can also operate above atmosphere"
  , flag' LiftingBody $ long "lifting-body" <> help "winged, reenters as unpowered glider"
  , flag' Capsule $ long "capsule" <> help "aerodynamic stable reentry with\
    \ negligible steering capability"
  , flag' Balloon $ long "balloon" <> help "gas propelled lighter-than-air,\
    \ hovers in stratosphere"]

parseClass :: Parser Class
parseClass = parserOptionGroup "Class" $ Class
  <$> asum
    [ flag VertLnch VertLnch $ long "vlnch" <> help "vertical launch orientation default"
    , flag' HorizLnch $ long "hlnch" <> help "horizontal launch orientation"
    , flag' PayldLnch $ long "plnch" <> help "payload launch orientation"]
  <*> asum [flag Air Air $ long "air" <> help "air launch domain default",
    flag' Gnd $ long "gnd" <> help "ground launch domain",
    flag' Sea $ long "sea" <> help "sea launch domain",
    flag' Spce $ long "spce" <> help "space launch domain"]
  <*> asum [flag VertLand VertLand $ long "vlnd" <>
    help "vertical landing orientation default",
    flag' HorizLand $ long "hlnd" <> help "horizontal landing orientation"]
  <*> asum [flag AirRec AirRec $ long "air-rec" <> help "air recovery default",
    flag' GndRec $ long "gnd-rec" <> help "ground recovery",
    flag' SeaRec $ long "sea-rec" <> help "sea recovery"]

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
  [ flag' Thrust $ long "thrust"
  , flag' OrdFTS $ long "ord-fts" <>
    help "Ordnance flight termination system (FTS)"
  , flag' FlightCtlManip $ long "flight-ctl-manip" <>
    help "Flight control manipulation"
  , flag' Other $ long "other"]

parsePt :: Parser (Double, Double)
parsePt = (,) <$> option auto (short 'x') <*> option auto (short 'y')

parseSiz :: Parser Size
parseSiz = Size <$> many parseWeight <*> many parseWeight <*>
  liftA2 (,) (option auto $ long "begin") (option auto $ long "end")
  <*> many parsePt

parseWeight :: Parser (Integer, (Double, Double))
parseWeight = (,) <$> option auto (long "stage-num") <*> parsePt

parseProp :: Parser Propellant
parseProp = Propellant <$> strOption (long "system-type") <*>
  option auto (long "fuel-cap") <*> option auto (long "oxidize") <*>
  many ((,) <$> strOption (long "other-ident") <*> option auto (long "other-cap"))

parsePayload :: Parser Payload
parsePayload = parserOptionGroup "Payload" $ asum [
  flag' Humans $ long "humans",
  flag' LiveAnimals $ long "live-animals",
  flag' LiveViableBiologics $ long "live-viable-bio",
  flag' NonLivingNonHazardCargo $ long "non-living-non-hazard",
  flag' HazardCargo $ long "hazard"]
