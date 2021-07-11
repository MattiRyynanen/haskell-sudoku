module PerHouseSolvers
(
    solveSingles,
    solveNakedPairs,
    solveHiddenPair,
    solveNakedTriplets,
    solveHiddenTriplet,
    findSingles,
    findNakedPairs
)
where

import qualified Data.Map as Map
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
findSingles = Map.keys . Map.filter (==1) . countOccurences
    . concatMap candidates . filter isUnsolved

-- Solving Naked pair
-- If row, column, or block contains two pairs with exactly same candidates,
-- the numbers in elsewhere within the house can be removed.

solveNakedPairs :: Transformer
solveNakedPairs puz = perHouseSolver puz searchNakedPair

searchNakedPair :: [Cell] -> (Cell -> Bool) -> [Transformer]
searchNakedPair puz p = map removerFor $ findNakedPairs house
    where house = filter p puz
          removerFor pair = applyWhen (\c -> p c && candidates c /= pair) (removeCellCandidates pair)

findNakedPairs :: [Cell] -> [[Candidate]]
findNakedPairs = Map.keys . Map.filter (==2) . countOccurences
    . filter hasTwo . map candidates

solveHiddenPair :: Transformer
solveHiddenPair puz = applyRemovers puz removers
    where ps = concatMap (searchHiddenPair puz) houseSelectors
          removerFor p = applyWhen (\c -> index c `elem` fst p) (setCellCandidates $ snd p)
          removers = map removerFor ps

searchHiddenPair :: Puzzle -> (Cell -> Bool) -> [([Index], [Candidate])]
searchHiddenPair puz selector = posCands
    where unsolved = filterWith [selector, isUnsolved] puz
          unique_cands = unique $ concatMap candidates unsolved
          positionsFor cand = map index $ filter (hasCand cand) unsolved
          twoPosCands = [(positionsFor cand, cand) | cand <- unique_cands, hasTwo $ positionsFor cand]
          unique_positions = unique $ map fst twoPosCands
          candsForP pos = map snd $ filter ((==pos) . fst) twoPosCands
          posCands = [(pos, candsForP pos) | pos <- unique_positions, hasTwo $ candsForP pos]

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
