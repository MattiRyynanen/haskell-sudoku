module Definitions where

import Snippets
import qualified Data.Char

type Index = Int
type Candidate = Int

data Cell = Cell {
    index :: Index,
    rowOf :: Index,
    colOf :: Index,
    blockOf :: Index,
    candidates :: [Candidate],
    broadcasted :: Bool -- if the final set value has been broadcasted to intersecting cells
    } deriving (Eq)

instance Show Cell where show c = concat [show $ index c, ":", concatMap show $ candidates c]

type Puzzle = [Cell]

createCell :: Index -> [Candidate] -> Cell
createCell i cands = Cell { index = i, rowOf = rowAt i, colOf = colAt i, blockOf = blockAt i, candidates = cands, broadcasted = False }

createEmptyPuzzle :: Puzzle
createEmptyPuzzle = [createCell i [1..9] | i <- [0..80]]

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

blockRowOf :: Cell -> Index
blockRowOf = (`div` 3) . blockOf

blockColOf :: Cell -> Index
blockColOf = (`rem` 3) . blockOf

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

keepOnlyCandidates :: [Candidate] -> Cell -> Cell
keepOnlyCandidates cands cell = cell { candidates = filter (`elem` cands) (candidates cell) }

setBroadcasted :: Cell -> Cell
setBroadcasted cell = cell { broadcasted = True }

hasCand :: Candidate -> Cell -> Bool
hasCand cand cell = cand `elem` candidates cell

hasNoCand :: Candidate -> Cell -> Bool
hasNoCand cand cell = cand `notElem` candidates cell

hasAnyCand :: [Candidate] -> Cell -> Bool
hasAnyCand cands cell = any (`hasCand` cell) cands

 -- Could also use but seems slower: any (($b) . ($a) . sameBy) [rowOf, colOf, blockOf]
intersects :: Cell -> Cell -> Bool
intersects a b = (rowOf a == rowOf b) || (colOf a == colOf b) || (blockOf a == blockOf b)

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
loadPuzzle str = zipWith createCellWith [0..] (filter (`elem` "123456789.") str)
    where createCellWith i n = createCell i (createCandidatesFrom n)
          createCandidatesFrom s = if s == '.' then [1..9] else [Data.Char.digitToInt s]
