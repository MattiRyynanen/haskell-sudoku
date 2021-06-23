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

applyRemover :: Puzzle -> (Puzzle -> Puzzle) -> Puzzle
applyRemover puzzle remover = remover puzzle

solveBlockOmissions :: Puzzle -> Puzzle
solveBlockOmissions puzzle = foldl applyRemover puzzle (rowRemovers ++ colRemovers)
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
solveOmitCandidateInOneBlock puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchOmitCandidateInOneBlock puzzle) (concatMap selectors [rowOf, colOf])
          selectors f = map (\i -> (==i) . f) [0..8]

searchOmitCandidateInOneBlock :: Puzzle -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchOmitCandidateInOneBlock puzzle p = removers
    where unsolved = filterWith [p, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint blockOf (withCandidate cand unsolved)
          removerFor blockInd cand = applyWhen (\c -> not (p c) && blockOf c == blockInd) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

-- Solving Naked pair
-- If row, column, or block contains two pairs with exactly same candidates,
-- the numbers in elsewhere within the section can be removed.

solveNakedPairs :: Puzzle -> Puzzle
solveNakedPairs puzzle = foldl applyRemover puzzle removers
    where sets = [(==i) . f | f <- [rowOf, colOf, blockOf], i <- [0..8]]
          removers = concatMap (searchNakedPair puzzle) sets

searchNakedPair :: [Cell] -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchNakedPair puzzle p = map removerFor $ filterWith [not . null, hasRemovals, isNakedPair] unique_pairs
    where unsolved = filterWith [p, isUnsolved] puzzle
          pair_cells = filter hasPair unsolved
          unique_pairs = unique $ map candidates pair_cells
          isNakedPair pair = hasTwo $ filter (==pair) $ map candidates pair_cells
          hasRemovals pair = any (\cell -> hasAnyCand pair cell && candidates cell /= pair) unsolved
          removerFor pair = applyWhen (\c -> p c && candidates c /= pair) (removeCellCandidates pair)

-- Double block omission.
-- On row or column of blocks:
-- for a candidate that is not solved in any of the blocks
-- if two of the blocks has candidate only on the same two rows (or, cols), the candidate
-- can be removed in the third block on those rows (columns).
