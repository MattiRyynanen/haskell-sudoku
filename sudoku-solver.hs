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

main = do
    putStrLn "Sudoku solver. Input a sudoku with a line and press enter"
    putStrLn ".8....3.1 .143..... ..5.7...9 5..6..1.. 42..5..87 ..8..7..5 2...8.5.. .....674. 8.6....1."
    interact (eachLine getSolution)
    putStrLn "Solver session finished."

-- Building profile
-- stack ghc -- -prof -fprof-auto -rtsopts sudoku-solver.hs

-- Running with profiling
-- cat puzzles.txt | ./sudoku-solver +RTS -p
