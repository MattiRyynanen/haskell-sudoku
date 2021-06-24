module Printers 
(
    printPuzzle,
    showPuzzle,
    tellCell,
    showSolutions
)
where 

import Data.List (intercalate)
import Definitions
import Snippets

showSolution :: (Puzzle, String, Puzzle) -> String
showSolution sol = concat [reason, " Unsolved cells = ", show $ length $ filter (not . isSolved) puzzle, "\n", showPuzzleChange puzzle prev, "\n"]
    where (puzzle, reason, prev) = sol

showSolutions :: [(Puzzle, String, Puzzle)] -> IO ()
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
showPuzzle cells = showPuzzleChange cells cells

showCellChange :: Cell -> Cell -> String
showCellChange cur prev = concatMap (showCandChange (candidates cur) (candidates prev)) [1..9]

showCandChange cur prev cand 
    | cand `elem` cur && hasOne cur = withColor 32 $ show cand
    | cand `elem` cur && hasTwo cur = withColor 33 $ show cand
    | cand `elem` cur = show cand
    | cand `elem` prev = withColor 31 $ show cand
    | hasOne cur && cur == prev = " "
    | otherwise = withColor 34 "."

showPuzzleChange :: Puzzle -> Puzzle -> String
showPuzzleChange cells previous = intercalate line (map concat (group 3 rows))
    where cellContents = [showCellChange c p | (c, p) <- zip cells previous]
          rows = map (('\n':) . intercalate "|") $ group 3 $ map unwords $ group 3 cellContents
          line = '\n' : intercalate "+" (replicate 3 (replicate 29 '-'))

printPuzzle :: Puzzle -> IO ()
printPuzzle cells = putStrLn $ showPuzzle cells

posOf :: Cell -> String
posOf c = concatMap (\f -> show $ f c) [rowOf, colOf, blockOf]

tellCell :: Cell -> String
tellCell c = unwords [posOf c, show (candidates c)]
