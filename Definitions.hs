module Definitions where

import Snippets
import qualified Data.Char

type Index = Int
type Candidate = Int
data Cell = Cell { index :: Index, candidates :: [Candidate] } deriving (Show, Eq)
type Puzzle = [Cell]

createEmptyPuzzle :: Puzzle
createEmptyPuzzle = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]

samePosWith :: Cell -> (Cell -> Bool)
samePosWith = sameBy index

sameBy :: Eq a => (Cell -> a) -> Cell -> Cell -> Bool
sameBy f x = (== f x) . f

rowAt :: Index -> Index
rowAt = (`div` 9)

colAt :: Index -> Index
colAt = (`rem` 9)

blockAt :: Index -> Index
blockAt ind = 3 * (rowAt ind `div` 3) + colAt ind `div` 3

rowOf :: Cell -> Index
rowOf = rowAt . index

colOf :: Cell -> Index
colOf = colAt . index

blockOf :: Cell -> Index
blockOf = blockAt . index

blockRowOf :: Cell -> Index
blockRowOf = (`div` 3) . blockOf

blockColOf :: Cell -> Index
blockColOf = (`rem` 3) . blockOf

numCandidates :: Cell -> Int
numCandidates = length . candidates

uniqueCandidates :: [Cell] -> [Candidate]
uniqueCandidates = unique . concatMap candidates

isSolved :: Cell -> Bool
isSolved = hasOne . candidates

isUnsolved :: Cell -> Bool
isUnsolved = not . hasOne . candidates

hasPair :: Cell -> Bool
hasPair = hasTwo . candidates

removeCellCandidate :: Candidate -> Cell -> Cell
removeCellCandidate cand cell = cell { candidates = filter (/= cand) (candidates cell) }

removeCellCandidates :: [Candidate] -> Cell -> Cell
removeCellCandidates cands cell = cell { candidates = filter (`notElem` cands) (candidates cell) }

setCellCandidate :: Candidate -> Cell -> Cell
setCellCandidate cand cell = cell { candidates = [cand] }

setCellCandidates :: [Candidate] -> Cell -> Cell
setCellCandidates cands cell = cell { candidates = cands }

hasCand :: Candidate -> Cell -> Bool
hasCand cand cell = cand `elem` candidates cell

hasNoCand :: Candidate -> Cell -> Bool
hasNoCand cand cell = cand `notElem` candidates cell

hasAnyCand :: [Candidate] -> Cell -> Bool
hasAnyCand cands cell = any (`hasCand` cell) cands

countCandidates :: Candidate -> [Cell] -> Int
countCandidates cand = length . filter (hasCand cand)

removeCandidateAt :: Candidate -> Index -> Puzzle -> Puzzle
removeCandidateAt cand ind = applyWhen ((==ind) . index) (removeCellCandidate cand)

setSolvedAt :: Candidate -> Index -> Puzzle -> Puzzle
setSolvedAt cand ind = applyWhen ((==ind) . index) (setCellCandidate cand)

intersects :: Cell -> Cell -> Bool
intersects a b = any (($b) . ($a) . sameBy) [rowOf, colOf, blockOf]

intersectsEx :: Cell -> Cell -> Bool
intersectsEx a b = index a /= index b && intersects a b

withCandidate :: Candidate -> [Cell] -> [Cell]
withCandidate = filter . hasCand

joint :: (Cell -> Index) -> [Cell] -> Maybe Index
joint f cells
    | allSame indx = Just $ head indx
    | otherwise = Nothing
    where indx = map f cells

loadPuzzle :: String -> Puzzle
loadPuzzle = zipWith createCell [0..]
    where createCell i n = Cell {index = i, candidates = createCandidatesFrom n}
          createCandidatesFrom s = if s == '.' then [1..9] else [Data.Char.digitToInt s]
