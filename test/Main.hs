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
  <*> genList (Gen.choice [pure AntiColisn, pure Proplsv
    , StatPwrFlight <$> genDbl, FastSlowApprchLndingSpd <$> genPlot <*> genDbl
    , GearCfg <$> genStr, JetiBdy <$> genStr, Stge <$> genInt <*> Gen.bool
      <*> Gen.maybe Gen.bool, TermSys <$> Gen.choice [pure Thrustt, pure OrdFTS
      , pure FlightCtlManip, Ots <$> genStr]])
  <*> genList (Gen.choice [LnchSz <$> genInt <*> genDbl <*> genDbl
    , RntrySz <$> genInt <*> genDbl <*> genDbl, Explosv <$> genDbl <*> genDbl
    , Thrustp <$> genPlot])
  <*> genList (Gen.choice [Sys <$> genStr, Fuel <$> genStr <*> genDbl
    , Oxidizr <$> genStr <*> genDbl, OtherProp <$> genStr <*> genDbl])
  <*> genList (Gen.choice [Human <$> genStr, Animal <$> genStr
    , Biologic <$> genStr, Cargo <$> genStr, Hazard <$> genStr])
  <*> genList (Gen.choice [Orb <$> genStr <*> genPlot
    , Suborb <$> genStr <*> genPlot, Windw <$> genStr <*> genInt <*> genInt
    , Flex <$> genStr <*> genInt, AdvNoticeOp <$> genStr <*> genDbl])

genPlot :: MonadGen m => m Plot
genPlot = genList $ (,) <$> genDbl <*> genDbl

genInt :: MonadGen m => m Int
genInt = Gen.int $ Range.linearFrom 0 (-2^(29 :: Integer)) (2^(29 :: Integer)-1)

genDbl :: MonadGen m => m Double
genDbl = Gen.double $ Range.linearFracFrom 0 (-999999999999) 999999999999

genStr :: MonadGen m => m String
genStr = Gen.string rangeList Gen.unicode

genList :: MonadGen m => m a -> m [a]
genList = Gen.list rangeList

rangeList :: Range Int
rangeList = Range.linearFrom 0 0 255
