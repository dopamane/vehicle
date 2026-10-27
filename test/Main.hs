module Main (main) where

import Data.Binary
import Data.Vehicle
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Data.Vehicle" [binTripTest]

binTripTest :: TestTree
binTripTest = testProperty "BinTrip" $ property $ do
  v <- forAll genVehicle
  tripping v encode $ \bs -> case decodeOrFail bs of
    Left  (_, _, er) -> Left er
    Right (_, _, v') -> Right (v' :: Vehicle)

genVehicle :: MonadGen m => m Vehicle
genVehicle = undefined
