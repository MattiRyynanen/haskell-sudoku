module Solvers where

import Definitions
import Snippets
import Printers
import Data.Maybe

-- Remove candidates based on already solved cells.

removeSolved :: Puzzle -> Puzzle
removeSolved puzzle = map removeCandidates puzzle
    where solved = filter isSolved puzzle
          candidatesToRemoveFor cell = concatMap candidates $ filter (intersectsEx cell) solved
          removeCandidates cell = foldl (flip removeCellCandidate) cell (candidatesToRemoveFor cell)

-- Only possibility solver:

solveOnlyPossibility :: Puzzle -> Puzzle
solveOnlyPossibility puzzle = foldl solveOnlyPossibilityAt puzzle [0..80]

solveOnlyPossibilityAt :: Puzzle -> Index -> Puzzle
solveOnlyPossibilityAt puzzle ind
    | isSolved cell = puzzle
    | length onlies == 1 = setSolvedAt (head onlies) ind puzzle
    | length onlies > 1 = error $ unwords ["Found more than one possible final candidate: ", tellCell cell, showPuzzle puzzle]
    | otherwise = puzzle
    where onlies = searchOnlyPossibilityAt puzzle ind (candidates cell)
          cell = puzzle !! ind

searchOnlyPossibilityAt :: Puzzle -> Int -> [Candidate] -> [Candidate]
searchOnlyPossibilityAt puzzle ind = filter (isOnlyPossibilityAt puzzle ind)

isOnlyPossibilityAt :: Puzzle -> Int -> Candidate -> Bool
isOnlyPossibilityAt puzzle ind cand
    | hasNoCand cand (puzzle !! ind) = error "Candidate not in the cell."
    | otherwise = any (onlyOneIn . get) [sameRow, sameCol, sameBlock]
    where sameRow = (== rowAt ind) . rowOf
          sameCol = (== colAt ind) . colOf
          sameBlock = (== blockAt ind) . blockOf
          get = flip filter puzzle
          onlyOneIn = hasOne . filter (==cand) . concatMap candidates

-- Block omission, candidates within one block on one row or column, only.
-- Can remove the possible candidates on that row or column on adjacent block.

solveBlockOmissions :: Puzzle -> Puzzle
solveBlockOmissions puzzle = foldr (\r p -> r p) puzzle (rowRemovers ++ colRemovers)
    where rowRemovers = concatMap (searchBlockOmissionBy rowOf puzzle) [0..8]
          colRemovers = concatMap (searchBlockOmissionBy colOf puzzle) [0..8]

searchBlockOmissionBy :: (Cell -> Index) -> Puzzle -> Index -> [Puzzle -> Puzzle]
searchBlockOmissionBy indexer puzzle blockIndex = removers
    where unsolved = filterWith [(==blockIndex) . blockOf, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint indexer (withCandidate cand unsolved)
          removerFor ind cand = applyWhen (\c -> indexer c == ind && blockOf c /= blockIndex) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

-- Block omission: row or column has a candidate only within one block, any other
-- candidate in that block can be removed.

solveOmitCandidateInOneBlock :: Puzzle -> Puzzle
solveOmitCandidateInOneBlock puzzle = foldr (\r p -> r p) puzzle removers
    where removers = concatMap (searchOmitCandidateInOneBlock puzzle) (concatMap selectors [rowOf, colOf])
          selectors f = map (\i -> (==i) . f) [0..8]

searchOmitCandidateInOneBlock :: Puzzle -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchOmitCandidateInOneBlock puzzle p = removers
    where unsolved = filterWith [p, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint blockOf (withCandidate cand unsolved)
          removerFor blockInd cand = applyWhen (\c -> not (p c) && blockOf c == blockInd) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

{-
solveOmissionWithinBlock :: Puzzle -> Puzzle
solveOmissionWithinBlock cells
    | null oms = cells
    | otherwise = applyWhenIndex (\i -> elem i removalInds) (\cell -> withNo candidateToRemove cell) cells
    where rowsAndCols = concat [map rowIndices [0..8], map colIndices [0..8]]
          oms = concat [searchOmissionWithinBlock cand indx cells | cand <- [1..9], indx <- rowsAndCols]
          (candidateToRemove, removalInds) = head oms

searchOmissionWithinBlock :: Int -> [Index] -> Puzzle -> [(Candidate, [Index])]
searchOmissionWithinBlock cand indx cells = if withinOneBlock && length r > 0 then [(cand, r)] else []
    where candInds = [i | (c, i) <- zip (getAt indx cells) indx, hasCand cand c]
          blockIds = unique $ map (blockOf) candInds
          withinOneBlock = length blockIds == 1
          bi = blockIndices (head blockIds)
          r = [i | (c, i) <- zip (getAt bi cells) bi, hasCand cand c, not $ elem i indx]
-}