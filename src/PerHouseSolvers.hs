module PerHouseSolvers
(
    solveSingles,
    solveNakedPairs,
    solveHiddenPairs,
    solveNakedTriplets,
    solveHiddenTriplet,
    findSingles,
    findNakedPairs,
    findHiddenPairs,
    candidatePosMap
)
where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap

import Snippets
import Definitions
import SolverDefinitions

perHouseSolver :: Puzzle -> (Puzzle -> (Cell -> Bool) -> [Transformer]) -> Puzzle
perHouseSolver puz houseTransformer = applyRemovers puz removers
    where removers = concatMap (houseTransformer puz) houseSelectors

-- Singles, only possibility solver:

solveSingles :: Transformer
solveSingles puz = perHouseSolver puz searchSingles

searchSingles :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchSingles puz p = map removerFor $ findSingles house
    where house = filter p puz
          removerFor cand = applyWhen (\c -> p c && hasCand cand c) (setCellCandidate cand)

findSingles :: [Cell] -> [Candidate]
findSingles = Map.keys . Map.filter (==1) . countOccurrences
    . concatMap candidates . filter isUnsolved

-- Solving Naked pair
-- If row, column, or block contains two pairs with exactly same candidates,
-- the numbers in elsewhere within the house can be removed.

solveNakedPairs :: Transformer
solveNakedPairs puz = perHouseSolver puz searchNakedPair

searchNakedPair :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchNakedPair puz p = map removerFor $ findNakedPairs house
    where house = filter p puz
          removerFor pair = applyWhen (\c -> p c && candidates c /= pair) (removeCellCandidates pair)

{- | findNakedPairs returns pairs appearing twice in a house.

>>> findNakedPairs $ createCells [[1,3],[1,5],[1,3],[1,4,5]]
[[1,3]]
-}

findNakedPairs :: [Cell] -> [[Candidate]]
findNakedPairs = Map.keys . Map.filter (==2) . countOccurrences
    . filter hasTwo . map candidates

-- Hidden pairs: two candidates in a house appear only in the same two locations.

solveHiddenPairs :: Transformer
solveHiddenPairs puz = perHouseSolver puz searchHiddenPairs

searchHiddenPairs :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchHiddenPairs puz p = map removerFor $ findHiddenPairs house
    where house = filter p puz
          removerFor pc = applyWhen (\c -> p c && index c `elem` fst pc) (setCellCandidates $ snd pc)

{- | findHiddenPairs returns a list of (positions, candidates)
where the candidates only appear.

>>> let house = createCells [[2,3,4], [3,4,5], [5,6], [8]]
>>> house
>>> findHiddenPairs house
[00:234,01:345,02:56,03:8]
[([0,1],[3,4])]
-}

findHiddenPairs :: [Cell] -> [([Index], [Candidate])]
findHiddenPairs cells = posCands
    where candMap = candidatePosMap cells
          uniquePositions = unique $ IntMap.elems $ IntMap.filter hasTwo candMap
          candsWithPos pos = IntMap.keys $ IntMap.filter (== pos) candMap
          posCands = [(pos, candsWithPos pos) | pos <- uniquePositions, hasTwo $ candsWithPos pos]

{- | Creates a map of candidates as keys and list of their positions as values.

>>> candidatePosMap $ createCells [[2,3,4], [3,4,5], [8]]
fromList [(2,[0]),(3,[0,1]),(4,[0,1]),(5,[1]),(8,[2])]
-}
candidatePosMap :: Foldable t => t Cell -> IntMap.IntMap [Index]
candidatePosMap = foldl addCell IntMap.empty
    where addCell m cell = foldl (addCand $ index cell) m (candidates cell)
          addCand pos m cand = IntMap.insertWith (flip (++)) cand [pos] m

-- Naked triplets.

solveNakedTriplets :: Transformer
solveNakedTriplets puz = perHouseSolver puz searchNakedTriplets

searchNakedTriplets :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchNakedTriplets puz p
    | length unsolved <= 3 = []
    | otherwise = map removerFor validCombs
    where unsolved = filterWith [p, isUnsolved] puz
          combinations = tripletCombinations unsolved
          hasThreeCandidates comb = (==3) $ length $ candidatesIn comb
          candidatesIn comb = unique $ concatMap candidates comb
          validCombs = filter hasThreeCandidates combinations
          indicesFor comb = map index comb
          removerFor comb = applyWhen (\c -> p c && index c `notElem` indicesFor comb) (removeCellCandidates (candidatesIn comb))

solveHiddenTriplet :: Transformer
solveHiddenTriplet puz = perHouseSolver puz searchHiddenTriplet

searchHiddenTriplet :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchHiddenTriplet puz p
    | length unsolved <= 3 = []
    | otherwise = map removerFor validCombs
    where unsolved = filterWith [p, isUnsolved] puz
          unique_cands = unique $ concatMap candidates unsolved
          positionsFor cand = map index $ filter (hasCand cand) unsolved
          tripletPosCands = [(positionsFor cand, cand) | cand <- unique_cands, (<=3) $ length $ positionsFor cand]
          combs = tripletCombinations tripletPosCands
          combIndx comb = unique $ concatMap fst comb
          validCombs = filter ((==3) . length . combIndx) combs
          indicesFor comb = unique $ concatMap fst comb
          candidatesIn comb = unique $ map snd comb
          removerFor comb = applyWhen (\c -> p c && index c `elem` indicesFor comb) (keepOnlyCandidates (candidatesIn comb))
