module PerHouseSolversSpec (spec) where

import Hudoku.Definitions ( createCells )
import Hudoku.PerHouseSolvers
import Test.Hspec

import qualified Data.IntMap.Strict as IntMap

spec :: Spec
spec = do
    describe "PerHouseSolvers.findSingles" $ do
        it "returns candidates which occur once in (house) unsolved cells" $ do
            findSingles (createCells [[1,2,3],[2,3,4],[5]]) `shouldBe` [1, 4, 5]

    describe "PerHouseSolvers.findNakedPairs" $ do
        it "returns pairs occurring twice" $ do
            findNakedPairs (createCells [[1,2],[2,3,4],[1,2],[5],[6,9],[6,9]])
            `shouldBe` [[1,2], [6,9]]

    describe "PerHouseSolvers.findHiddenPairs" $ do
        it "returns indices where two candidates appear" $ do
            findHiddenPairs (createCells [[2,3,5], [2,5,6], [8]])
            `shouldBe` [([0, 1], [2, 5])]

    describe "PerHouseSolvers.candidatePosMap" $ do
        it "creates candidates as key with positions as value" $ do
            candidatePosMap (createCells [[2,3,5], [2,5,6], [8]])
            `shouldBe` IntMap.fromList [(2, [0, 1]), (3, [0]), (5, [0, 1]), (6, [1]), (8, [2])]
