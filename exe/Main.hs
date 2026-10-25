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
parseLaunchDom = undefined

parseLandingOrien :: Parser LandingOrientation
parseLandingOrien = undefined

parseRecovery :: Parser Recovery
parseRecovery = undefined

parseMode :: Parser Mode
parseMode = undefined

parseCap :: Parser Capability
parseCap = undefined

parseSiz :: Parser Size
parseSiz = undefined

parseProp :: Parser Propellant
parseProp = undefined

parsePayload :: Parser Payload
parsePayload = undefined
