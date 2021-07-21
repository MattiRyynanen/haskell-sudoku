module Hudoku.Printers 
(
    showPuzzle,
    showSolutions,
    showPuzzleNoCands
)
where 

import Data.List (intercalate)
import Data.Maybe
import Hudoku.Definitions
import Hudoku.Snippets
import Hudoku.SolverDefinitions

showSolution :: Int -> SolutionStep -> String
showSolution minLevel (SolverStep res prev solv) = concat [show solv, " Unsolved cells = ", unsolved, puzzleStr]
    where unsolved = show $ length $ filter isUnsolved res
          puzzleStr = if level solv <= minLevel then "" else '\n' : showPuzzleChange res prev ++ "\n"

showSolution _ (IdleStep puz stepId) = concat ["Step: ", show stepId, "\n", colorPz]
    where pz = showPuzzleNoCands puz
          colorPz = case stepId of Solved -> withColor (32 :: Int) pz
                                   NoSolution -> showPuzzle puz
                                   _ -> pz

showSolutions :: [SolutionStep] -> String
showSolutions xs = intercalate "\n" $ map (showSolution 1) (reverse xs)

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = concat ["\ESC[", show c, "m", str, "\ESC[0m"]

showPuzzleNoCands :: Puzzle -> String
showPuzzleNoCands p = intercalate line (map concat (group 3 rows))
    where rows = map (('\n':) . intercalate "|") $ group 3 $ map concat $ group 3 cells
          cells = map cellContent p
          cellNumber c = if isSolved c then show $ head $ candidates c else "."
          cellContent c = concat [" ", cellNumber c, " "]
          line = '\n' : intercalate "+" (replicate 3 (replicate 9 '-'))

showPuzzle :: Puzzle -> String
showPuzzle cells = showPuzzleChange cells cells

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
    | isJust prev && isNothing cur = end ++ cand
    | isNothing prev && isJust cur = start cur ++ cand
    | otherwise = end ++ start cur ++ cand
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
showPuzzleChange cells prev = intercalate line (map concat (group 3 rows))
    where cellContents = [showCellChange2 c p | (c, p) <- zip cells prev]
          rows = map (('\n':) . intercalate "|") $ group 3 $ map unwords $ group 3 cellContents
          line = '\n' : intercalate "+" (replicate 3 (replicate 29 '-'))
