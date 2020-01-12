module Data.LibSpec (spec) where

import Test.Hspec
import Data.Lib

spec :: Spec
spec =
  describe "Lib module" $
    it "#foo" $
      foo "bar" `shouldBe` "Foo: bar"

