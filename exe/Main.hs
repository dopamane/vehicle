module Main (main) where

import Data.Binary
import Data.Binary.Get
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

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Space Launch & Reentry Vehicles"

data CLI = ReadVehicle (Maybe FilePath)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [command "r" $ info (ReadVehicle <$> optional fileArg) $ progDesc "Read vehicle data"]

fileArg :: Parser FilePath
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN"
