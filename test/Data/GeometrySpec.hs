module Data.GeometrySpec (spec) where

import Test.Hspec
import Data.Geometry

spec :: Spec
spec = do
  describe "Geometry" $ do
    it "sphereVolume" $ do
      sphereVolume 10 `shouldBe` (4.0 / 3.0) * pi * 10 ^ 3
    it "sphereArea" $ do
      sphereArea 10 `shouldBe` 4 * pi * 10 ^ 2
    it "cubeVolume" $ do
      cubeVolume 10 `shouldBe` 10 ^ 3
    it "cubeArea" $ do
      cubeArea 10 `shouldBe` 10 ^ 2 * 6
    it "cuboidVolume" $ do
      cuboidVolume 10 20 30 `shouldBe` 10 * 20 * 30
    it "cuboidArea" $ do
      cuboidArea 10 20 30 `shouldBe` 10 * 20 * 2 + 20 * 30 * 2 + 10 * 30 * 2
