import Data.Maybe
import System.Environment

import Definitions
import Printers
import Solvers

dispatch :: [(String, [String] -> IO ())]
dispatch = [("session", session), ("help", help)]

main = do
    cargs <- getArgs
    if null cargs
        then do help []
        else do
            let (Just action) = lookup (head cargs) dispatch
            action (tail cargs)

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

session :: [String] -> IO ()
session _ = do
    putStrLn "Sudoku solver. Input a sudoku with a line and press enter."
    putStrLn "Example:"
    putStrLn ".8....3.1 .143..... ..5.7...9 5..6..1.. 42..5..87 ..8..7..5 2...8.5.. .....674. 8.6....1."
    interact (eachLine getSolution)
    putStrLn "Solver session finished."

help :: [String] -> IO ()
help _ = do
    putStrLn "A help text here."

-- Building profile
-- stack ghc -- -prof -fprof-auto -rtsopts sudoku-solver.hs

-- Running with profiling
-- cat puzzles.txt | ./sudoku-solver +RTS -p
