module Definitions where

import Snippets ( hasOne, hasTwo, unique, allSame, elemInOrdered )
import Data.Char ( digitToInt )

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

instance Show Cell where show c = concat [show $ rowOf c, show $ colOf c, ":", concatMap show $ candidates c]

data Puzzle = Puzzle { pcells :: [Cell], unsolvedRows :: [Index], unsolvedCols :: [Index], unsolvedBlocks :: [Index] } deriving (Show)

{- Creates a sudoku cell with given position index and candidates.

>>> createCell 0 [1,2,3]
00:123
-}
createCell :: Index -> [Candidate] -> Cell
createCell i cands = Cell { index = i, rowOf = rowAt i, colOf = colAt i, blockOf = blockAt i, candidates = cands, broadcasted = False }

{- Creates sudoku cells with candidates and increasing position index.

>>> createCells [[1,2,3],[4,5],[6]]
[00:123,01:45,02:6]
-}
createCells :: [[Candidate]] -> [Cell]
createCells = zipWith createCell [0 ..]

{- | Creates an empty 9x9 Sudoku puzzle cells.

>>> length $ pcells createEmptyPuzzle
81
-}
createEmptyPuzzle :: Puzzle
createEmptyPuzzle = createPuzzleWithCells (createCells $ replicate (9 * 9) [1 .. 9])

createPuzzleWithCells :: [Cell] -> Puzzle
createPuzzleWithCells cells = Puzzle { pcells = cells, unsolvedRows = [0..8], unsolvedCols = [0..8], unsolvedBlocks = [0..8] }

{-| Returns True if cells have the same index.

>>> map (samePosWith (createCell 0 [1])) $ createCells [[1], [1,2]]
[True,False]
-}
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

houseSelectors :: [Cell -> Bool]
houseSelectors = [(==i) . f | f <- [rowOf, colOf, blockOf], i <- [0..8]]

uniqueCandidates :: [Cell] -> [Candidate]
uniqueCandidates = unique . concatMap candidates

isSolved :: Cell -> Bool
isSolved = hasOne . candidates

isUnsolved :: Cell -> Bool
isUnsolved = not . isSolved

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
hasCand cand = elemInOrdered cand . candidates

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

housesOk :: Puzzle -> Bool
housesOk puzzle = all isValid houseSelectors
    where solved = filter isSolved $ pcells puzzle
          isValid house = let b = take 9 $ filter house solved in (length b == uniqueCands b)
          uniqueCands = length . unique . map candidates

hasZeroCandidates :: [Cell] -> Bool
hasZeroCandidates = any (null . candidates)

updateCells :: Puzzle -> [Cell] -> Puzzle
updateCells puz cells = puz { pcells = cells }

loadPuzzle :: String -> Maybe Puzzle
loadPuzzle str
    | length (pcells puzzle) == 81 && housesOk puzzle = Just puzzle
    | otherwise = Nothing
    where createCellWith i n = createCell i (createCandidatesFrom n)
          createCandidatesFrom s = if s == '.' then [1 .. 9] else [digitToInt s]
          puzzle = createPuzzleWithCells $ zipWith createCellWith [0 ..] (filter (`elem` "123456789.") str) :: Puzzle
