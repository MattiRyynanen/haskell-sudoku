module PerHouseSolvers where

import Snippets
import Definitions
import SolverDefinitions

-- Singles, only possibility solver:

solveSingles :: Transformer
solveSingles puz = applyRemovers puz removers
    where removers = concatMap (searchSingles puz) houseSelectors

searchSingles :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchSingles puz p = map removerFor singles
    where unsolved = filterWith [p, isUnsolved] puz
          singles = filter isSingle $ uniqueCandidates unsolved
          isSingle cand = hasOne $ withCandidate cand unsolved
          removerFor cand = applyWhen (\c -> p c && hasCand cand c) (setCellCandidate cand)

-- Solving Naked pair
-- If row, column, or block contains two pairs with exactly same candidates,
-- the numbers in elsewhere within the house can be removed.

solveNakedPairs :: Transformer
solveNakedPairs puz = applyRemovers puz removers
    where removers = concatMap (searchNakedPair puz) houseSelectors

searchNakedPair :: [Cell] -> (Cell -> Bool) -> [Transformer]
searchNakedPair puz p = map removerFor $ filterWith [not . null, hasRemovals, isNakedPair] unique_pairs
    where unsolved = filterWith [p, isUnsolved] puz
          pair_cells = filter hasPair unsolved
          unique_pairs = unique $ map candidates pair_cells
          isNakedPair pair = hasTwo $ filter (==pair) $ map candidates pair_cells
          hasRemovals pair = any (\cell -> hasAnyCand pair cell && candidates cell /= pair) unsolved
          removerFor pair = applyWhen (\c -> p c && candidates c /= pair) (removeCellCandidates pair)

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
solveNakedTriplets puz = applyRemovers puz removers
    where removers = concatMap (searchNakedTriplets puz) houseSelectors

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
solveHiddenTriplet puz = applyRemovers puz removers
    where removers = concatMap (searchHiddenTriplet puz) houseSelectors

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
