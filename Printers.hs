module Printers 
(
    printPuzzle,
    noHighlights,
    showPuzzle,
    tellCell,
    showSolutions
)
where 

import Data.List (intercalate)
import Definitions
import Snippets

noHighlights :: [Bool]
noHighlights = replicate (9 * 9) False

showSolution :: (Puzzle, String, [Bool]) -> String
showSolution sol = concat [reason, " Unsolved cells = ", show $ length $ filter (not . isSolved) puzzle, "\n", showPuzzleHighlights puzzle highlights, "\n"]
    where (puzzle, reason, highlights) = sol

showSolutions :: [(Puzzle, String, [Bool])] -> IO ()
showSolutions xs = mapM_ (putStrLn . showSolution) (reverse xs)

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = concat ["\ESC[", show c, "m", str, "\ESC[0m"]

showCell :: Cell -> String
showCell c
    | numCandidates c == 1 = withColor 32 (cellToStr " " c)
    | numCandidates c == 2 = withColor 33 (cellToStr "." c)
    | otherwise = cellToStr "." c
    where cellToStr sep c = concat $ [if hasCand cand c then show cand else sep | cand <- [1..9]]

showPuzzle :: Puzzle -> String
showPuzzle cells = showPuzzleHighlights cells noHighlights

showPuzzleHighlights :: Puzzle -> [Bool] -> String
showPuzzleHighlights cells highlights = intercalate line (map concat (group 3 rows))
    where cellContents = [if h then withColor 44 (showCell c) else showCell c | (c, h) <- zip cells highlights]
          rows = map (('\n':) . intercalate "|") $ group 3 $ map unwords $ group 3 cellContents
          line = '\n' : intercalate "+" (replicate 3 (replicate 29 '-'))

printPuzzle :: Puzzle -> IO ()
printPuzzle cells = putStrLn $ showPuzzle cells

posOf :: Cell -> String
posOf c = concatMap (\f -> show $ f c) [rowOf, colOf, blockOf]

tellCell :: Cell -> String
tellCell c = unwords [posOf c, show (candidates c)]