module SnippetsSpec (spec) where

import Snippets
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.List (nub)
import qualified Data.Map as Map

prop_hasOne :: [Int] -> Bool
prop_hasOne xs = hasOne xs == (==1) (length xs)

prop_allSame :: [Int] -> Property
prop_allSame xs = not (null xs) ==> allSame xs == (==1) (length (nub xs))

spec :: Spec
spec = do
    describe "Snippets.hasOne" $ do
        prop "is equal to length being one" prop_hasOne
        it "returns True if list has one element, otherwise False" $ do
            map hasOne ["", "a", "ab"] `shouldBe` [False, True, False]

    describe "Snippets.allSame ([a])" $ do
        it "returns True if all list elements are equal" $ do
            allSame ["a", "a", "a"] `shouldBe` True

        prop "is same as having only one unique element" prop_allSame

    describe "Snippet.countOccurrences" $ do
        it "Counts occurrences" $ do
            Map.toList (countOccurences "abcb") `shouldBe` [('a',1), ('b',2), ('c',1)]
