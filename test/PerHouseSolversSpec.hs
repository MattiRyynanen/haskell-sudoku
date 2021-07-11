module PerHouseSolversSpec (spec) where

import Definitions ( createCell, Candidate, Cell )
import PerHouseSolvers
import Test.Hspec

import qualified Data.IntMap.Strict as IntMap

createHouse :: [[Candidate]] -> [Cell]
createHouse = zipWith createCell [0 ..]

spec :: Spec
spec = do
    describe "PerHouseSolvers.findSingles" $ do
        it "returns candidates which occur once in (house) unsolved cells" $ do
            findSingles (createHouse [[1,2,3],[2,3,4],[5]]) `shouldBe` [1, 4]

    describe "PerHouseSolvers.findNakedPairs" $ do
        it "returns pairs occurring twice" $ do
            findNakedPairs (createHouse [[1,2],[2,3,4],[1,2],[5],[6,9],[6,9]])
            `shouldBe` [[1,2], [6,9]]

    describe "PerHouseSolvers.findHiddenPairs" $ do
        it "returns indices where two candidates appear" $ do
            findHiddenPairs (createHouse [[2,3,5], [2,5,6], [8]])
            `shouldBe` [([0, 1], [2, 5])]

    describe "PerHouseSolvers.candidatePosMap" $ do
        it "creates candidates as key with positions as value" $ do
            candidatePosMap (createHouse [[2,3,5], [2,5,6], [8]])
            `shouldBe` IntMap.fromList [(2, [0, 1]), (3, [0]), (5, [0, 1]), (6, [1]), (8, [2])]
