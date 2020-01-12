module Data.NatSpec (spec) where

import Data.Nat
import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "instance Num Nat" $ do
    it "fromInteger 0" $ do
      (fromInteger 0 :: Nat) `shouldBe` Zerro
    it "fromInteger 1" $ do
      (fromInteger 1 :: Nat) `shouldBe` Succ (Zerro)
    it "fromInteger 3" $ do
      (fromInteger 3 :: Nat) `shouldBe` Succ (Succ (Succ (Zerro)))
    it "+" $ do
      (fromInteger 2 :: Nat) + (fromInteger 3 :: Nat)
        `shouldBe` (fromInteger 5 :: Nat)
    it "*" $ do
      (fromInteger 2 :: Nat) * (fromInteger 3 :: Nat)
        `shouldBe` (fromInteger 6 :: Nat)
    it "abs" $ do
      abs (fromInteger 2 :: Nat) `shouldBe` (fromInteger 2 :: Nat)
    it "signum Zerro" $ do
      signum Zerro `shouldBe` Zerro
    it "signum Succ (Succ (Zerro))" $ do
      signum (fromInteger 3 :: Nat) `shouldBe` Succ (Zerro)
    it "negate undefined for Nat" $ do
      evaluate (negate (Succ (Zerro)))
        `shouldThrow` errorCall "negate undefined for Nat"
  describe "Helpers" $ do
    it "emptyNat" $ do
      emptyNat `shouldBe` Zerro
    it "singletonNat" $ do
      singletonNat `shouldBe` Succ (Zerro)
  describe "beside" $ do
    it "when not beside" $ do
      beside (fromInteger 1 :: Nat) (fromInteger 3 :: Nat) `shouldBe` False
    it "when beside left" $ do
      beside (fromInteger 2 :: Nat) (fromInteger 3 :: Nat) `shouldBe` True
    it "when beside right" $ do
      beside (fromInteger 3 :: Nat) (fromInteger 2 :: Nat) `shouldBe` True



