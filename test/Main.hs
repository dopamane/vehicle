module Main (main) where

import Data.Binary
import Data.Vehicle
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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
genVehicle = Vehicle
  <$> Gen.choice [Gov <$> genStr, Com <$> genStr]
  <*> Gen.choice [GovGov <$> genStr, ComGov <$> genStr, ComCom <$> genStr
    , GovCom <$> genStr]
  <*> Gen.enumBounded
  <*> genList (Gen.choice [LnchOri <$> Gen.enumBounded
    , LnchDom <$> Gen.enumBounded, LndOri <$> Gen.enumBounded
    , Recov <$> Gen.enumBounded])
  <*> Gen.enumBounded
  <*> pure []
  <*> pure []
  <*> pure []
  <*> genList (Gen.choice [Human <$> genStr, Animal <$> genStr
    , Biologic <$> genStr, Cargo <$> genStr, Hazard <$> genStr])
  <*> pure []

genStr :: MonadGen m => m String
genStr = Gen.string rangeList Gen.unicode

genList :: MonadGen m => m a -> m [a]
genList = Gen.list rangeList

rangeList :: Range Int
rangeList = Range.linearFrom 0 0 255
