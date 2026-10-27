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
  [command "r" $ info (fmap ReadVehicle $
    optional $ fileArg "Optional binary input file otherwise stream STDIN") $
    progDesc "Read vehicle data"
  , command "w" $ info (WriteVehicle <$> parseVehicle <*>
    optional (fileArg "Optional binary output file otherwise stream STDOUT")) $
    progDesc "Write vehicle data"]

fileArg :: String -> Parser FilePath
fileArg h = strArgument $ metavar "FILE" <> completer (bashCompleter "file") <> help h

parseVehicle :: Parser Vehicle
parseVehicle = Vehicle <$> parseReg <*> parseMissnTy <*> parseCat <*> many parseClass
  <*> parseMode <*> many parseCap <*> many parseSiz <*> many parseProp
  <*> many parsePayld <*> many parseMissn

parseReg :: Parser Reg
parseReg = parserOptionGroup "Regulatory type" $
  fmap Gov (strOption $ long "gov" <> metavar "STR" <> help "Government") <|>
  fmap Com (strOption $ long "com" <> metavar "STR" <> help "Commerical")

parseMissnTy :: Parser MissnTy
parseMissnTy = parserOptionGroup "Mission Type" $ asum
  [ fmap GovGov $ strOption $ long "gov-gov" <> metavar "STR" <>
    help "Government for Government"
  , fmap ComGov $ strOption $ long "com-gov" <> metavar "STR" <>
    help "Commerical for Government"
  , fmap ComCom $ strOption $ long "com-com" <> metavar "STR" <>
    help "Commerical for Commercial"
  , fmap GovCom $ strOption $ long "gov-com" <> metavar "STR" <>
    help "Government for Commerical"
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
parseClass = parserOptionGroup "Class" $ asum
  [ LnchOri <$> asum
    [ flag' VertLnch $ long "vlnch" <> help "vertical launch orientation"
    , flag' HorizLnch $ long "hlnch" <> help "horizontal launch orientation"
    , flag' PayldLnch $ long "plnch" <> help "payload launch orientation"]
  , LnchDom <$> asum
    [ flag' Air $ long "air" <> help "air launch domain"
    , flag' Gnd $ long "gnd" <> help "ground launch domain"
    , flag' Sea $ long "sea" <> help "sea launch domain"
    , flag' Spce $ long "spce" <> help "space launch domain"]
  , LndOri <$> asum
    [ flag' VertLand $ long "vlnd" <> help "vertical landing orientation"
    , flag' HorizLand $ long "hlnd" <> help "horizontal landing orientation"]
  , Recov <$> asum
    [ flag' AirRec $ long "air-rec" <> help "air recovery"
    , flag' GndRec $ long "gnd-rec" <> help "ground recovery"
    , flag' SeaRec $ long "sea-rec" <> help "sea recovery"]]

parseMode :: Parser Mode
parseMode = parserOptionGroup "Mode" $ asum
  [ flag Piloted Piloted $ long "piloted" <> help "flight ctl by human default"
  , flag' UnguidedBallistic $ long "unguided-ballistic" <>
    help "flight path det by environ factors"
  , flag' AutoGuided $ long "auto-guided" <> help "flight ctl by onboard computa"
  , flag' GuidedExtCmds $ long "guided-ext" <>
    help "flight guided by computer & external cmds"
  , flag' AutoPilotHybrid $ long "auto-pilot-hybrid" <>
    help "flight ctl by human and/or computer"
  ]

parseCap :: Parser Cap
parseCap = parserOptionGroup "Capability" $ asum
  [ flag' AntiColisn $ long "acolisn" <> help "anti-collision sense & avoid"
  , flag' Proplsv $ long "proplsv" <> help "propulsive mode other than attitude ctl"
  , fmap StatPwrFlight $ option auto $ long "spf" <> metavar "DBL"
    <> help "Stationary powered flight duration seconds"
  , FastSlowApprchLndingSpd <$>
    many ((,) <$> option auto (short 'a' <> metavar "DBL" <> help "altitude")
              <*> option auto (short 's' <> metavar "DBL" <> help "speed"))
    <*> option auto (long "lnding-spd" <> metavar "DBL" <> help "final landing speed")
  , GearCfg <$> strOption (long "gear-cfg" <> metavar "STR" <> help "landing gear config")
  , JetiBdy <$> strOption (long "jeti-bdy" <> metavar "STR" <> help "jettison body")
  , Stge <$> option auto (long "stge" <> metavar "INT" <> help "stage num") <*>
    switch (long "ctl-rntry" <> help "controlled reentry") <*>
    optional (flag' True (long "reuse" <> help "reusable") <|>
      flag' False (long "no-reuse" <> help "non-reusable"))
  , TermSys <$> asum
    [ flag' Thrustt $ long "thrst" <> help "thrust termination system"
    , flag' OrdFTS $ long "ofts" <> help "ordnance flight termination system (FTS)"
    , flag' FlightCtlManip $ long "fcm" <> help "flight control manipulation termination system"
    , fmap Ots $ strOption $ long "ots" <> metavar "STR" <>
      help "other termination system"]]

parseSiz :: Parser Size
parseSiz = parserOptionGroup "Size" $ asum
  [ LnchSz <$> option auto (long "lnch-stg" <> metavar "INT" <> help "launch stage")
    <*> option auto (long "lnch-bgn" <> metavar "DBL" <> help "launch weight begin kg")
    <*> option auto (long "lnch-end" <> metavar "DBL" <> help "launch weight end kg")
  , RntrySz <$> option auto (long "rntry-stg" <> metavar "INT" <> help "reentry stage")
    <*> option auto (long "rntry-bgn" <> metavar "DBL" <> help "reentry weight begin kg")
    <*> option auto (long "rntry-end" <> metavar "DBL" <> help "reentry weight end kg")
  , Explosv <$> option auto (long "explo-bgn" <> metavar "DBL" <> help "explosive begin TNT")
    <*> option auto (long "explo-end" <> metavar "DBL" <> help "explosive end TNT")
  , fmap Thrustp $ some $ (,)
    <$> option auto (short 't' <> metavar "DBL" <> help "thrust profile time")
    <*> option auto (short 'n' <> metavar "DBL" <> help "thrust profile N")]

parseProp :: Parser Prop
parseProp = parserOptionGroup "Propellant" $ asum
  [ fmap Sys $ strOption $ long "sys" <> metavar "STR" <> help "system type"
  , Fuel <$> strOption (long "fuel" <> metavar "STR" <> help "fuel type")
    <*> option auto (long "fuel-cap" <> metavar "DBL" <> help "fuel capacity kg")
  , Oxidizr <$> strOption (long "oxid" <> metavar "STR" <> help "oxidizer type")
    <*> option auto (long "oxid-cap" <> metavar "DBL" <> help "oxidizer capacity kg")
  , OtherProp <$> strOption (long "other-prop" <> metavar "STR" <> help "other propellant type")
    <*> option auto (long "other-cap" <> metavar "DBL" <> help "other propellant capacity kg")
  ]

parsePayld :: Parser Payld
parsePayld = parserOptionGroup "Payload" $ asum
  [ fmap Human $ strOption $ long "human" <> metavar "STR" <> help "humans"
  , fmap Animal $ strOption $ long "animal" <> metavar "STR" <> help "animals"
  , fmap Biologic $ strOption $ long "biologic" <> metavar "STR" <> help "plants etc."
  , fmap Cargo $ strOption $ long "cargo" <> metavar "STR" <> help "non-hazard cargo"
  , fmap Hazard $ strOption $ long "hazard" <> metavar "STR" <> help "hazardous cargo"]

parseMissn :: Parser Missn
parseMissn = parserOptionGroup "Mission" $ asum
  [ fmap Orb $ some $ (,)
    <$> strOption (long "orb" <> metavar "STR" <> help "orbital plot descriptor")
    <*> many ((,) <$> option auto (short 'a' <> metavar "DBL" <> help "orbital altitude")
                  <*> option auto (short 't' <> metavar "DBL" <> help "orbital time"))
  , Suborb <$> strOption (long "suborb" <> metavar "STR" <> help "suborbital plot descriptor")
    <*> many ((,) <$> option auto (short 'a' <> metavar "DBL" <> help "suborbital altitude")
                  <*> option auto (short 't' <> metavar "DBL" <> help "suborbital time"))
  , fmap NASUsage $ some $ NAS
    <$> strOption (long "nas-desc" <> metavar "STR" <> help "NAS descriptor")
    <*> option auto (long "nas-strt" <> metavar "INT" <> help "NAS start MET")
    <*> option auto (long "nas-end" <> metavar "INT" <> help  "NAS end MET")
    <*> many ((,) <$> option auto (long "lon" <> metavar "DBL" <> help "NAS longitude")
                  <*> option auto (long "lat" <> metavar "DBL" <> help "NAS latitude"))
  , Windw <$> option auto (long "windw" <> metavar "STR" <> help "window descriptor")
    <*> option auto (long "hs" <> metavar "INT" <> help "window duration hours")
    <*> option auto (long "ms" <> metavar "INT" <> help "window duration minutes")
  , flag' TgtFlex $ long "tgt-flex" <> help "target flex"
  , flag' AdvNoticeOp $ long "adv-notice-op" <> help "advance notice operations"]
