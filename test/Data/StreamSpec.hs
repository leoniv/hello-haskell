module Data.StreamSpec (spec) where

import Test.Hspec
import qualified Data.Stream as S

spec :: Spec
spec = do

  describe "instace Show" $ do
    it "show" $ do
      show (S.constStream 10) `shouldBe` "[10,10,10..."

  describe "Stream elements access" $ do
    it "head" $ do
      S.head (S.constStream 10) `shouldBe` 10
    it "tail" $ do
      S.take 3 (S.tail ds10) `shouldBe` [9,8,7]
    it "(!!)" $ do
      (discendStream 10 S.!! 3) `shouldBe` 7
    it "take" $ do
      S.take 5 (discendStream 10) `shouldBe` [10, 9 .. 6]
  describe "Stream converters" $ do
    it "map" $ do
      S.take 5 (S.map negate (discendStream 10)) `shouldBe` [-10, -9 .. -6]
    it "filter" $ do
      S.take 6 (S.filter (<0) (discendStream 10)) `shouldBe` [-1, -2 .. -6]
    it "zip" $ do
      S.take 3 (S.zip ds10 ds11) `shouldBe` [(10,11),(9,10),(8,9)]

  describe "Stream generators" $ do
    it "iterate" $ do
      S.take 3 (S.iterate (*2) 1) `shouldBe` [1,2,4]

discendStream :: Integer -> S.Stream Integer
discendStream i = i S.:& discendStream (i - 1)

ds10 :: S.Stream Integer
ds10 = discendStream 10

ds11 :: S.Stream Integer
ds11 = discendStream 11
