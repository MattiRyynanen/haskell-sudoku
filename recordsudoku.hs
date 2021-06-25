import Data.List (intercalate)
import qualified Data.Char
import qualified SamplePuzzles
import Definitions
import Snippets
import Printers
import Solvers

pl = loadPuzzle SamplePuzzles.level5_hs_20200619

solutions :: [(Puzzle, String, Puzzle)]
solutions = solve [(pl, "The loaded puzzle.", pl)]

(px, _, _) = head solutions

applyWhileReduced f puzzle =
    let reduced = f puzzle
    in if reduced == puzzle then puzzle else applyWhileReduced f reduced

solve puzzles
    | all isSolved p = (p, "Solved!", p) : puzzles -- solved
    | p /= a = solve ((a, "Removed candidates by solved values.", p) : puzzles)
    | p /= b = solve ((b, "Only possible candidate.", p) : puzzles)
    | p /= bo = solve ((bo, "Omission: candidates in block on same row or column.", p) : puzzles)
    | p /= oib = solve ((oib, "Omission: candidates within one block.", p) : puzzles )
    | p /= c = solve ((c, "Naked pairs.", p) : puzzles)
    | p /= hp = solve ((hp, "Hidden pairs.", p) : puzzles)
    | otherwise = (p, "No solution yet.", p) : puzzles -- no solution
    where (p, _, _) = head puzzles
          a = applyWhileReduced removeSolved p
          b = solveOnlyPossibility p
          c = solveNakedPairs p
          bo = solveBlockOmissions p
          oib = solveOmitCandidateInOneBlock p
          hp = solveHiddenPair p
