import Data.List (intercalate)
import qualified Data.Char
import qualified SamplePuzzles
import Definitions
import Snippets
import Printers
import Solvers

pl = loadPuzzle SamplePuzzles.xtr_sud_19

solutions :: [(Puzzle, String, [Bool])]
solutions = solve [(pl, "The loaded puzzle.", noHighlights)]

(px, _, _) = head solutions

applyWhileReduced f puzzle =
    let reduced = f puzzle
    in if reduced == puzzle then puzzle else applyWhileReduced f reduced

solve puzzles
    | all isSolved p = (p, "Solved!", noHighlights) : puzzles -- solved
    | p /= a = solve ((a, "Removed candidates by solved values.", elemDiff p a) : puzzles)
    | p /= b = solve ((b, "Only possible candidate.", elemDiff p b) : puzzles)
    | p /= bo = solve ((bo, "Omission: candidates in block on same row or column.", elemDiff p bo) : puzzles)
    | p /= oib = solve ((oib, "Omission: candidates within one block.", elemDiff p oib) : puzzles )
--    | p /= c = solve ((c, "A naked pair.", elemDiff p c) : puzzles)
    | otherwise = (p, "No solution yet.", noHighlights) : puzzles -- no solution
    where (p, _, _) = head puzzles
          a = applyWhileReduced removeSolved p
          b = solveOnlyPossibility p
          --c = solveNakedPair p
          bo = solveBlockOmissions p
          oib = solveOmitCandidateInOneBlock p
