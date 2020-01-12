module Data.Geometry.TypesSpec (spec) where

import Test.Hspec
import qualified Data.Geometry.Types as T

spec :: Spec
spec = do
  describe "All types sould be Show'ing" $ do
    it "Point" $ do
      show (T.Point 1 1) `shouldBe` "Point 1.0 1.0"

    it "Shape.Circle" $ do
      show (T.baseCircle 10) `shouldBe` "Circle (Point 0.0 0.0) 10.0"

    it "Shape.Rectangle" $do
      show (T.baseRectangel (T.Point 10 10)) `shouldBe`
        "Rectangle (Point 0.0 0.0) (Point 10.0 10.0)"

  describe "Helpers" $ do
    it "baseCircle shord return Circle with (Point 0 0) and passed radius" $ do
      T.baseCircle 10 `shouldBe` (T.Circle (T.Point 0 0) 10)

    it "baseRectangel" $ do
      T.baseRectangel (T.Point 10 10) `shouldBe` (T.Rectangle (T.Point 0 0) (T.Point 10 10))
