import Data.List (intercalate)
import qualified Data.Char
import qualified SamplePuzzles
import Definitions
import Snippets
import Printers
import Solvers

solveOnlyPossibilityAt puzzle ind
    | isSolved cell = puzzle
    | length onlies == 1 = setSolvedAt (head onlies) ind puzzle
    | length onlies > 1 = error $ unwords ["Found more than one possible final candidate: ", tellCell cell, showPuzzle puzzle]
    | otherwise = puzzle
    where onlies = searchOnlyPossibilityAt puzzle ind (candidates cell)
          cell = puzzle !! ind

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

pl = loadPuzzle SamplePuzzles.xtr_sud_04

solutions :: [(Puzzle, String, [Bool])]
solutions = solve [(pl, "The loaded puzzle.", noHighlights)]

solve puzzles
    | all isSolved p = (p, "Solved!", noHighlights) : puzzles -- solved
    | p /= a = solve ((a, "Removed candidates by solved values.", elemDiff p a) : puzzles)
    | p /= b = solve ((b, "Only possible candidate.", elemDiff p b) : puzzles)
--    | p /= bo = solve ((bo, "Omission: candidates in block on same row or column.", elemDiff p bo) : puzzles)
--    | p /= oib = solve ((oib, "Omission: candidates within one block.", elemDiff p oib) : puzzles )
--    | p /= c = solve ((c, "A naked pair.", elemDiff p c) : puzzles)
    | otherwise = (p, "No solution yet.", noHighlights) : puzzles -- no solution
    where (p, _, _) = head puzzles
          a = removeSolved p
          b = foldl solveOnlyPossibilityAt p [0..80]
          --c = solveNakedPair p
          --bo = solveBlockOmission p
          --oib = solveOmissionWithinBlock p
