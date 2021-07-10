module PerHouseSolversSpec (spec) where

import Definitions
import PerHouseSolvers
import Test.Hspec

createHouse :: [[Candidate]] -> [Cell]
createHouse = zipWith createCell [0 ..]

spec :: Spec
spec = do
    describe "PerHouseSolvers.findSingles" $ do
        it "returns candidates which occur once in (house) unsolved cells" $ do
            findSingles (createHouse [[1,2,3],[2,3,4],[5]]) `shouldBe` [1, 4]
