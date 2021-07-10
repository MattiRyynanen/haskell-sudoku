module SnippetsSpec (spec) where

import Snippets
import Test.Hspec

spec :: Spec
spec = do
    describe "Snippets.hasOne" $ do

        it "returns True for lists with one element" $ do
            hasOne "a" `shouldBe` True

        it "returns False for empty lists" $ do
            hasOne "" `shouldBe` False

        it "returns False for lists longer than one" $ do
            hasOne "ab" `shouldBe` False
