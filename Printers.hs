module Printers 
(
    printPuzzle,
    showPuzzle,
    tellCell,
    showSolutions
)
where 

import Data.List (intercalate)
import Data.Maybe
import Definitions
import Snippets
import Solvers

showSolution :: Int -> SolutionStep -> String
showSolution minLevel step = concat [show solver, " Unsolved cells = ", unsolved, puzzleStr]
    where SolutionStep result previous solver = step
          unsolved = show $ length $ filter isUnsolved result
          puzzleStr = if (level solver) <= minLevel then "" else '\n' : (showPuzzleChange result previous) ++ "\n"

showSolutions :: [SolutionStep] -> IO ()
showSolutions xs = mapM_ (putStrLn . (showSolution 1)) (reverse xs)

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = concat ["\ESC[", show c, "m", str, "\ESC[0m"]

showPuzzle :: Puzzle -> String
showPuzzle cells = showPuzzleChange cells cells

showCellChange :: Cell -> Cell -> String
showCellChange cur prev = concatMap (showCandChange (candidates cur) (candidates prev)) [1..9]

showCandChange :: [Candidate] -> [Candidate] -> Candidate -> String
showCandChange cur prev cand 
    | cand `elem` cur && hasOne cur = withColor 32 $ show cand
    | cand `elem` cur && hasTwo cur = withColor 33 $ show cand
    | cand `elem` cur = show cand
    | cand `elem` prev = withColor 31 $ show cand
    | hasOne cur && cur == prev = " "
    | otherwise = withColor 34 "."

showCellChange2 :: Cell -> Cell -> String
showCellChange2 cur prev = combineColored (candidates cur) (candidates prev)

combineColored :: [Candidate] -> [Candidate] -> String
combineColored cur prev = colored ++ "\ESC[0m"
    where colors = map (getColor cur prev) [1..9]
          chars = map (getCandChar cur prev) [1..9]
          prevColors = Nothing : colors
          colored = concat $ zipWith3 candStr prevColors colors chars

candStr :: Maybe Int -> Maybe Int -> String -> String
candStr prev cur cand
    | cur == prev = cand
    | isJust prev && isNothing cur = concat [end, cand]
    | isNothing prev && isJust cur = concat [start cur, cand]
    | otherwise = concat [end, start cur, cand]
    where end = "\ESC[0m"
          start c = concat ["\ESC[", show $ fromJust c, "m"]

getCandChar :: [Candidate] -> [Candidate] -> Candidate -> String
getCandChar cur prev cand
    | any (elem cand) [cur, prev] = show cand
    | hasOne cur && cur == prev = " "
    | otherwise = "."

getColor :: [Candidate] -> [Candidate] -> Candidate -> Maybe Int
getColor cur prev cand
    | currentCandidate && hasOne cur = Just 32 -- green, solved value in solved cell
    | currentCandidate && hasTwo cur = Just 33 -- yellow, pair in cell
    | currentCandidate = Nothing -- normal candidate, no coloring
    | cand `elem` prev = Just 31 -- red, candidate was in previous so it was removed
    | hasOne cur && cur == prev = Nothing -- solved cell, no change
    | otherwise = Just 34 -- blue for empty space dots
    where currentCandidate = cand `elem` cur

showPuzzleChange :: Puzzle -> Puzzle -> String
showPuzzleChange cells previous = intercalate line (map concat (group 3 rows))
    where cellContents = [showCellChange2 c p | (c, p) <- zip cells previous]
          rows = map (('\n':) . intercalate "|") $ group 3 $ map unwords $ group 3 cellContents
          line = '\n' : intercalate "+" (replicate 3 (replicate 29 '-'))

printPuzzle :: Puzzle -> IO ()
printPuzzle cells = putStrLn $ showPuzzle cells

posOf :: Cell -> String
posOf c = concatMap (\f -> show $ f c) [rowOf, colOf, blockOf]

tellCell :: Cell -> String
tellCell c = unwords [posOf c, show (candidates c)]
