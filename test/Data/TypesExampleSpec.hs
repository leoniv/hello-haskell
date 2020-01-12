module Data.TypesExampleSpec (spec) where

import Test.Hspec
import Data.TypesExample

book = (Book SuperCover [Page "1", Page "2"])

spec :: Spec
spec = do
  describe "Book" $ do
    it "pageCount" $ do
      pageCount book `shouldBe` 2
    it "pageContent" $ do
      pageContent book 1 `shouldBe` "1"
    it "pageGet" $ do
      (show . pageGet book $ 1) `shouldBe` "1"

  describe "Show Book" $ do
    it "show" $ do
      show book `shouldBe` "1 2"


