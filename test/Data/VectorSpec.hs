module Data.VectorSpec (spec) where

import Test.Hspec
import Data.Vector as V

spec :: Spec
spec = do
  describe "Vector" $ do
    it "show" $ do
      show (V.Vector 1 2 3) `shouldBe` "Vector 1 2 3"
    it "vplus" $ do
      (V.Vector 1 2 3 `V.vplus` Vector 10 20 30) `shouldBe` Vector 11 22 33
    it "scalarProd" $ do
      (V.Vector 2 3 4) `V.scalarProd` (V.Vector 3 4 5) `shouldBe` 6 + 12 + 20
    it "vmult" $ do
      (V.Vector 2 3 4) `V.vmult` (V.Vector 3 4 5) `shouldBe` V.Vector 6 12 20

