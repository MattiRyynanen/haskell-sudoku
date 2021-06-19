module Solvers (removeSolved, solveOnlyPossibility) where

import Definitions
import Snippets
import Printers

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
