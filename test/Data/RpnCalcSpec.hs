module Data.RpnCalcSpec (spec) where

import Test.Hspec
import Data.RpnCalc

spec :: Spec
spec = do
  describe "solveRpn" $ do
    it "4 2 3 + *" $ do
      solveRpn "4 2 3 + *" `shouldBe` Just 20
    it "100 3 * 12 +" $ do
      solveRpn "100 3 * 12 +" `shouldBe` Just 312
    it "100 30 - 10 *" $ do
      solveRpn "100 30 - 10 *" `shouldBe` Just (-700)
    it "100 ln" $ do
      solveRpn "100 ln" `shouldBe` Just (log 100)
    it "3 2 ^" $ do
      solveRpn "3 2 ^" `shouldBe` Just 8
    it "empty" $ do
      solveRpn "" `shouldBe` Nothing
    it "inavlid" $ do
      solveRpn "invalid" `shouldBe` Nothing

