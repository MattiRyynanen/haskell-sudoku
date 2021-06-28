import Data.List (intercalate, find)
import qualified Data.Char
import Data.Maybe

import qualified SamplePuzzles
import Definitions
import Snippets
import Printers
import Solvers

pl = loadPuzzle SamplePuzzles.level5_hs2020_07_04 --level5_hs_20200619
solutions = solve [idleStep pl "The loaded puzzle."]

px = result $ head solutions

applyWhileReduced f puzzle =
    let reduced = f puzzle
    in if reduced == puzzle then puzzle else applyWhileReduced f reduced

solvers = let s = Solver in [
    s (applyWhileReduced removeSolved) "Removed candidates by solved values." 0,
    s solveOnlyPossibility "Only possible candidate." 0,
    s solveBlockOmissions "Omission: candidates in block on same row or column." 1,
    s solveOmitCandidateInOneBlock "Omission: candidates within one block." 1,
    s solveNakedPairs "Naked pairs." 2,
    s solveHiddenPair "Hidden pairs." 2,
    s solveNakedTriplets "Naked triplets." 3,
    s solveHiddenTriplet "Hidden triplets." 4,
    s solveXwing "X-Wing." 5]

idSolver n = Solver id n 100
idleStep puzzle id = SolutionStep puzzle puzzle (idSolver id)

solve :: [SolutionStep] -> [SolutionStep]
solve steps
    | null steps = error "Nothing to solve."
    | all isSolved latest = prepend $ idleStep latest "Solved!"
    | isJust simplestSolver = solve $ prepend $ stepFor simplestSolver
    | otherwise = prepend $ idleStep latest "No solution yet."
    where latest = result $ head steps -- the latest puzzle
          simplestSolver = find (\solver -> transformer solver latest /= latest) solvers
          prepend step = step : steps
          stepFor solver = let s = fromJust solver in SolutionStep (transformer s latest) latest s
