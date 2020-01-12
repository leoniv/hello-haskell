module Data.PersonSpec (spec) where

import Test.Hspec
import Data.Person

spec :: Spec
spec = do
  describe "Rerson deriving (Show, Eq, Read)" $ do
    it "Eq" $ do
      personNew "f" "l" 10 `shouldBe` personNew "f" "l" 10
    it "Show" $ do
      show (personNew "f" "l" 10) `shouldBe`
        "Person {firstName = \"f\", lastName = \"l\", age = 10}"
    it "Read" $ do
      (read (show (personNew "f" "l" 10)) :: Person) `shouldBe`
        personNew "f" "l" 10
