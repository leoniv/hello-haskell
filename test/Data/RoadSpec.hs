module Data.RoadSpec (spec) where

import Data.Road
import Test.Hspec

spec :: Spec
spec = do
  describe "roadStep" $ do
    it "first step" $ do
      roadStep ([], []) (head heathrowLondon) `shouldBe`
        ([(C, 30), (B, 10)], [(B, 10)])
  describe "optimalPath" $ do
    it "for heathrowLondon road" $ do
      optimalPath heathrowLondon `shouldBe`
        [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8), (C, 0)]
