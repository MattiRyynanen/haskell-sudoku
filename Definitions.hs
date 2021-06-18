module Definitions (
    Index,
    Candidate,
    Cell(..),
    Puzzle,
    createEmptyPuzzle,
    removeCellCandidate, setCellCandidate,
    rowAt, colAt, blockAt,
    rowOf, colOf, blockOf,
    isSolved, isUnsolved, hasPair, numCandidates, hasCand, hasNoCand
) where

import Snippets

type Index = Int
type Candidate = Int
data Cell = Cell { index :: Index, candidates :: [Candidate] } deriving (Show, Eq)
type Puzzle = [Cell]

createEmptyPuzzle :: Puzzle
createEmptyPuzzle = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]

samePosWith :: Cell -> (Cell -> Bool)
samePosWith c = (== index c) . index

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

numCandidates :: Cell -> Int
numCandidates = length . candidates

isSolved :: Cell -> Bool
isSolved = hasOne . candidates

isUnsolved :: Cell -> Bool
isUnsolved = not . hasOne . candidates

hasPair :: Cell -> Bool
hasPair = hasTwo . candidates

removeCellCandidate :: Candidate -> Cell -> Cell
removeCellCandidate cand cell = cell { candidates = filter (/= cand) (candidates cell) }

setCellCandidate :: Candidate -> Cell -> Cell
setCellCandidate cand cell = cell { candidates = [cand] }

hasCand :: Candidate -> Cell -> Bool
hasCand cand cell = cand `elem` candidates cell

hasNoCand :: Candidate -> Cell -> Bool
hasNoCand cand cell = cand `notElem` candidates cell

countCandidates :: Candidate -> [Cell] -> Int
countCandidates cand = length . filter (hasCand cand)