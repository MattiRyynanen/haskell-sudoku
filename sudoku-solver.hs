import Definitions
import Printers
import Solvers

main = do
    putStrLn "Welcome to a Sudoku solver. Please enter a sudoku:"
    line <- getLine
    let pl = loadPuzzle line
        solutions = solve [idleStep pl "The loaded puzzle."]
    putStrLn $ showSolutions solutions
