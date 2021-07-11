module PerHouseSolversSpec (spec) where

import Definitions ( createCell, Candidate, Cell )
import PerHouseSolvers
import Test.Hspec

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
