module Data.TreeSpec (spec) where

import Data.Tree
import Test.Hspec

spec :: Spec
spec = do
  describe "Deriving" $ do
    it "show" $ do
      show (treeNew 10) `shouldBe` "Node 10 EmptyTree EmptyTree"

  describe "treeInsert" $ do
    it "less value is inserted in to left branch" $ do
      treeInsert 5 (treeNew 10) `shouldBe` Node 10 (treeNew 5) EmptyTree

    it "more value is inserted in to right branch" $ do
      treeInsert 15 (treeNew 10) `shouldBe` Node 10 EmptyTree (treeNew 15)

    it "equal value don't inserted" $ do
      treeInsert 10 (treeNew 10) `shouldBe` treeNew 10

    it "wen pass EmptyTree" $ do
      treeInsert 10 EmptyTree `shouldBe` treeNew 10

  describe "treeElem" $ do
    it "when pass EmptyTree" $ do
      treeElem 0 EmptyTree `shouldBe` False

    it "less value will be found in the left brahch" $ do
      treeElem 5 largeTree `shouldBe` True

    it "more value will be found in the right brahch" $ do
      treeElem 15 largeTree `shouldBe` True

    it "when value dosn't in the tree" $ do
      treeElem 25 (treeInsert 15 (treeNew 10)) `shouldBe` False

  describe "instance of Functor" $ do
    it "fmap when EmptyTree" $ do
      fmap (+1) EmptyTree `shouldBe` EmptyTree
    it "fmap when not EmptyTree" $ do
      fmap (Just) (Node 1 (EmptyTree) (Node 2 (EmptyTree) (EmptyTree))) `shouldBe` (Node (Just 1) (EmptyTree) (Node (Just 2) (EmptyTree) (EmptyTree)))
  describe "instance of Foldable" $ do
    it "fordMap" $ do
      foldMap (:[]) (Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)) `shouldBe` [2, 1, 3]
    it "fordr" $ do
      foldr (+) 10 (Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)) `shouldBe` 16
    it "toList" $ do
      toList (Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)) `shouldBe` [2,1,3]

largeTree =
  foldr treeInsert (treeNew 10) [1..20]
