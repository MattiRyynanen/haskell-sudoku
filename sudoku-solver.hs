import Definitions
import Printers
import Solvers

getSolution :: String -> String
getSolution line
    | null line = "-- empty line --"
    | head line == '#' = line
    | otherwise = showSolutions solutions
    where pl = loadPuzzle line
          solutions = solve [idleStep pl "The loaded puzzle."]

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

main = interact (eachLine getSolution)
