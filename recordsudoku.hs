import qualified SamplePuzzles
import Definitions
import Printers
import Solvers

main = do
    let pl = loadPuzzle SamplePuzzles.level5_hs2020_07_04
        solutions = solve [idleStep pl "The loaded puzzle."]
    putStrLn $ showSolutions solutions
    --showSolutions solutions