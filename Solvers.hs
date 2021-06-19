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

-- Block omission

solveBlockOmissions :: Puzzle -> Puzzle
solveBlockOmissions puzzle = foldl (\p r -> r p) puzzle (rowRemovers ++ colRemovers)
    where rowRemovers = concatMap (searchBlockOmissionBy rowOf puzzle) [0..8]
          colRemovers = concatMap (searchBlockOmissionBy colOf puzzle) [0..8]

searchBlockOmissionBy :: (Cell -> Index) -> Puzzle -> Index -> [Puzzle -> Puzzle]
searchBlockOmissionBy indexer puzzle blockIndex = removers
    where unsolved = filter ((==blockIndex) . blockOf) $ filter isUnsolved puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint indexer (withCandidate cand unsolved)
          removerFor ind cand = applyWhen (\c -> indexer c == ind && blockOf c /= blockIndex) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

{-

solveBlockOmission :: Puzzle -> Puzzle
solveBlockOmission puzzle
    | null oms = puzzle -- no omissions found
    | otherwise = applyWhen ((`elem` removalInds) . index) (removeCellCandidate cand) puzzle
    where oms = concatMap (`searchBlockOmission` puzzle) [0..8]
          (cand, indx) = head oms
          h_indx = head indx
          blockIndex = blockAt h_indx
          inAnotherBlock ind = blockAt ind /= blockIndex
          f_indx = if isOnSameRow indx then rowIndicesAt else colIndicesAt
          removalInds = filter inAnotherBlock $ f_indx h_indx

searchBlockOmission :: Index -> Puzzle -> [(Candidate, [Index])]
searchBlockOmission blockIndex cells = [(cand, indx) | (cand, indx) <- zip candidates (map withCand candidates), hasRemovals cand indx]
    where bi = blockIndices blockIndex
          unsolved_cells = [(c, i) | (c, i) <- zip (getAt bi cells) bi, cellUnsolved c]
          candidates = uniqueCandidates $ map fst unsolved_cells
          withCand cand = [i | (c, i) <- unsolved_cells, hasCand cand c]
          inAnotherBlock ind = blockOf ind /= blockIndex
          hasRemovalCand indx c = any (hasCand c) $ getAt (filter inAnotherBlock indx) cells
          onSameRow c indx = isOnSameRow indx && hasRemovalCand (rowIndicesAt $ head indx) c
          onSameCol c indx = isOnSameCol indx && hasRemovalCand (colIndicesAt $ head indx) c
          hasRemovals cand indx = onSameRow cand indx || onSameCol cand indx

-}