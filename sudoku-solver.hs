import Data.Maybe

import Definitions
import Printers
import Solvers

getSolution :: String -> String
getSolution line
    | null line = "-- empty line --"
    | head line == '#' = line
    | isNothing pl = "Invalid puzzle."
    | otherwise = showSolutions solutions
    where pl = loadPuzzle line
          solutions = solve [IdleStep (fromJust pl) Initial]

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

main = interact (eachLine getSolution)

-- Building profile
-- stack ghc -- -prof -fprof-auto -rtsopts sudoku-solver.hs

-- Running with profiling
-- cat puzzles.txt | ./sudoku-solver +RTS -p
