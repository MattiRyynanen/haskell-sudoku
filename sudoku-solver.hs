import Data.Maybe
import System.Environment

import Definitions
import Printers
import Solvers

type Program = [String] -> IO ()

dispatch :: [(String, Program)]
dispatch = [ ("session", session)
           , ("help", help)
           , ("stats", stats)
           ]

main = do
    cargs <- getArgs
    if null cargs
        then do help []
        else do
            let (Just action) = lookup (head cargs) dispatch
            action (tail cargs)

help :: Program
help _ = do
    putStrLn "Please provide program: help, session"

session :: Program
session _ = do
    putStrLn "Sudoku solver. Input a sudoku with a line and press enter."
    putStrLn "Example:"
    putStrLn ".8....3.1 .143..... ..5.7...9 5..6..1.. 42..5..87 ..8..7..5 2...8.5.. .....674. 8.6....1."
    interact (eachLine getSolution)
    putStrLn "Solver session finished."

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

stats :: Program
stats _ = do
    contents <- getContents
    putStrLn (solveAllFrom contents)

solveAllFrom input = result
    where puzzles = mapMaybe loadPuzzle (lines input)
          result = concat ["Found ", show $ length puzzles, " puzzles."]

-- Building profile
-- stack ghc -- -prof -fprof-auto -rtsopts sudoku-solver.hs

-- Running with profiling
-- cat puzzles.txt | ./sudoku-solver +RTS -p
